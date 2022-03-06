use core::fmt;
use proc_macro2::{Literal, Span, TokenStream, TokenTree};
use proc_macro_error::{abort, abort_call_site, emit_error, proc_macro_error, Diagnostic};
use quote::quote;
use syn::parse::{Parse, ParseStream, Parser};
use syn::{punctuated::Punctuated, spanned::Spanned, Token};

mod impl_enum;
mod impl_struct;

#[proc_macro_error]
#[proc_macro_derive(Bytecode, attributes(bytecode))]
pub fn bytecode_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse(input).unwrap_or_else(|e| Diagnostic::from(e).abort());
    impl_bytecode(&ast).into()
}

struct SpannedValue<T> {
    value: T,
    span: Span,
}

impl<T> SpannedValue<T> {
    fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

#[derive(Clone, Copy)]
enum InstructionType {
    U8,
    U16,
    U32,
    U64,
}

struct FieldAttributes {
    skip: bool,
    flatten: Option<SpannedValue<Punctuated<syn::Pat, Token![,]>>>,
    flatten_all: Option<SpannedValue<Punctuated<syn::Pat, Token![,]>>>,
}

impl FieldAttributes {
    fn parse(attrs: &[syn::Attribute]) -> Self {
        let mut skip = None;
        let mut flatten = None;
        let mut flatten_all = None;
        parse_attributes(attrs, |item| match item.name.to_string().as_str() {
            "skip" => {
                if item.value.is_some() {
                    emit_error!(
                        item.value.span(),
                        "The `skip` attribute must not have a value"
                    );
                }
                match &mut skip {
                    Some(_) => emit_error!(item.name.span(), "Duplicate attribute `{}`", item.name),
                    None => skip = Some(true),
                }
            }
            "flatten" => match item.value {
                Some(TokenTree::Group(v)) => {
                    if v.delimiter() != proc_macro2::Delimiter::Bracket {
                        emit_error!(v.span(), "Expected bracket delimiter");
                    }
                    let parser = Punctuated::<syn::Pat, Token![,]>::parse_terminated;
                    let patterns = parser
                        .parse2(v.stream())
                        .unwrap_or_else(|e| Diagnostic::from(e).abort());
                    match &mut flatten {
                        Some(_) => {
                            emit_error!(item.name.span(), "Duplicate attribute `{}`", item.name)
                        }
                        None => flatten = Some(SpannedValue::new(patterns, v.span())),
                    }
                }
                _ => emit_error!(item.value.span(), "Expected list of values"),
            },
            "flatten_all" => match item.value {
                Some(TokenTree::Group(v)) => {
                    if v.delimiter() != proc_macro2::Delimiter::Bracket {
                        emit_error!(v.span(), "Expected bracket delimiter");
                    }
                    let parser = Punctuated::<syn::Pat, Token![,]>::parse_terminated;
                    let patterns = parser
                        .parse2(v.stream())
                        .unwrap_or_else(|e| Diagnostic::from(e).abort());
                    match &mut flatten_all {
                        Some(_) => {
                            emit_error!(item.name.span(), "Duplicate attribute `{}`", item.name)
                        }
                        None => flatten_all = Some(SpannedValue::new(patterns, v.span())),
                    }
                }
                _ => emit_error!(item.value.span(), "Expected list of values"),
            },
            _ => emit_error!(item.name.span(), "Unknown attribute `{}`", item.name),
        });
        Self {
            skip: skip.unwrap_or(false),
            flatten,
            flatten_all,
        }
    }
}

enum Fields {
    Named(Vec<NamedField>),
    Unnamed(Vec<UnnamedField>),
}

struct NamedField {
    ident: syn::Ident,
    type_: syn::Type,
    attrs: FieldAttributes,
}

impl NamedField {
    fn binding_ident(&self) -> syn::Ident {
        syn::Ident::new(&format!("__self_{}", self.ident), self.ident.span())
    }
}

struct UnnamedField {
    index: usize,
    type_: syn::Type,
    attrs: FieldAttributes,
}

impl UnnamedField {
    fn binding_ident(&self) -> syn::Ident {
        syn::Ident::new(&format!("__self_{}", self.index), self.type_.span())
    }
}

impl InstructionType {
    fn type_ident(self) -> syn::Ident {
        let s = match self {
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
        };
        syn::Ident::new(s, Span::call_site())
    }

    fn casted_literal(self, value: usize) -> Option<Literal> {
        Some(match self {
            Self::U8 => Literal::u8_suffixed(value.try_into().ok()?),
            Self::U16 => Literal::u16_suffixed(value.try_into().ok()?),
            Self::U32 => Literal::u32_suffixed(value.try_into().ok()?),
            Self::U64 => Literal::u64_suffixed(value.try_into().ok()?),
        })
    }

    const fn num_bytes(self) -> usize {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
        }
    }
}

impl fmt::Display for InstructionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
        })
    }
}

struct AttributeItem {
    name: syn::Ident,
    value: Option<TokenTree>,
}

impl Parse for AttributeItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // NOTE: For some reason, `input.parse::<syn::Ident>()` results in an error here, so instead
        //       the token is parsed into a `TokenTree`.
        let ident_token_tree = input.parse::<TokenTree>()?;
        let name = match ident_token_tree {
            TokenTree::Ident(v) => v,
            _ => {
                return Err(syn::Error::new_spanned(
                    ident_token_tree,
                    "Expected identifier",
                ))
            }
        };

        let value = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Some(input.parse::<TokenTree>()?)
        } else {
            None
        };
        Ok(Self { name, value })
    }
}

struct AttributeItems(Punctuated<AttributeItem, Token![,]>);

impl Parse for AttributeItems {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(Punctuated::parse_terminated(input)?))
    }
}

fn parse_attributes<F>(attrs: &[syn::Attribute], mut f: F)
where
    F: FnMut(AttributeItem),
{
    for attr in attrs {
        if attr.path.is_ident("bytecode") {
            let items: AttributeItems = attr
                .parse_args()
                .unwrap_or_else(|e| Diagnostic::from(e).abort());
            for item in items.0 {
                f(item);
            }
        }
    }
}

fn impl_bytecode(ast: &syn::DeriveInput) -> TokenStream {
    let mut type_ = None;
    parse_attributes(&ast.attrs, |item| match item.name.to_string().as_str() {
        "type" => {
            let item_value_span = item.value.span();
            match item.value {
                Some(TokenTree::Ident(ident)) => {
                    let instruction_type = match ident.to_string().as_str() {
                        "u8" => InstructionType::U8,
                        "u16" => InstructionType::U16,
                        "u32" => InstructionType::U32,
                        "u64" => InstructionType::U64,
                        _ => abort!(
                            "Invalid instruction type `{}`",
                            ident;
                            note = "Valid types are `u8`, `u16`, `u32`, and `u64`"
                        ),
                    };
                    match type_ {
                        Some(_) => {
                            emit_error!(item.name.span(), "Duplicate attribute `{}`", item.name)
                        }
                        None => type_ = Some(SpannedValue::new(instruction_type, item_value_span)),
                    }
                }
                _ => emit_error!(item.value.span(), "Expected identifier"),
            }
        }
        _ => emit_error!(item.name.span(), "Unknown attribute `{}`", item.name),
    });

    match &ast.data {
        syn::Data::Struct(v) => {
            if let Some(v) = type_ {
                emit_error!(v.span, "The attribute `type` is not allowed for structs");
            }
            impl_struct::impl_bytecode(&ast.ident, &ast.generics, &v.fields)
        }
        syn::Data::Enum(v) => {
            let type_ =
                type_.unwrap_or_else(|| abort_call_site!("Missing required attribute `type`"));
            impl_enum::impl_bytecode(&ast.ident, &ast.generics, type_, &v.variants)
        }
        syn::Data::Union(_) => abort!(
            ast.span(),
            "Union types are not supported by the `Bytecode` derive macro"
        ),
    }
}

fn buf_put_method_call(instruction_type: InstructionType, i: usize) -> TokenStream {
    let type_ident = instruction_type.type_ident();
    let method_ident = syn::Ident::new(&format!("put_{}", type_ident), Span::call_site());
    let literal = instruction_type.casted_literal(i);
    quote! { buf.#method_ident(#literal) }
}
