use crate::{
    parse_attributes, FieldAttributes, Fields, InstructionTypeTrait, NamedField, SpannedValue,
    UnnamedField,
};
use itertools::Itertools;
use proc_macro2::{Span, TokenStream, TokenTree};
use proc_macro_error::{abort, emit_error, Diagnostic};
use quote::{quote, ToTokens};
use std::collections::HashSet;
use syn::{parse::Parser, punctuated::Punctuated, Token};

#[derive(Clone)]
enum Pattern {
    Variable(TokenStream),
    Value(TokenStream),
}

struct Combination {
    match_pattern: TokenStream,
    encode_fields: Vec<TokenStream>,
    build_self: TokenStream,
}

struct VariantAttributes<T> {
    code: Option<SpannedValue<Vec<T>>>,
}

impl<T: InstructionTypeTrait> VariantAttributes<T> {
    fn parse(attrs: &[syn::Attribute]) -> Self {
        let mut code = None;
        parse_attributes(attrs, |item| match item.name.to_string().as_str() {
            "code" => {
                let attr = match item.value {
                    Some(TokenTree::Group(v)) => {
                        if v.delimiter() != proc_macro2::Delimiter::Bracket {
                            emit_error!(v.span(), "Expected bracket delimiter");
                        }
                        let parser = Punctuated::<syn::LitInt, Token![,]>::parse_terminated;
                        let ints = parser
                            .parse2(v.stream())
                            .unwrap_or_else(|e| Diagnostic::from(e).abort());
                        let value = ints
                            .into_iter()
                            .map(|v| {
                                T::literal_parse_base10(&v).unwrap_or_else(|_| {
                                    abort!(
                                        v.span(),
                                        "Expected `{}` integer",
                                        std::any::type_name::<T>()
                                    )
                                })
                            })
                            .collect();
                        SpannedValue::new(value, v.span())
                    }
                    Some(TokenTree::Literal(v)) => {
                        let int = syn::LitInt::from(v);
                        let value = T::literal_parse_base10(&int).unwrap_or_else(|_| {
                            abort!(
                                int.span(),
                                "Expected `{}` integer",
                                std::any::type_name::<T>()
                            )
                        });
                        SpannedValue::new(vec![value], int.span())
                    }
                    Some(_) => todo!(),
                    None => abort!(item.name.span(), "Expected value"),
                };
                match &mut code {
                    Some(_) => emit_error!(item.name.span(), "Duplicate attribute `{}`", item.name),
                    None => code = Some(attr),
                }
            }
            _ => emit_error!(item.name.span(), "Unknown attribute `{}`", item.name),
        });
        Self { code }
    }
}

pub(crate) fn impl_bytecode<T: InstructionTypeTrait>(
    ident: &syn::Ident,
    generics: &syn::Generics,
    variants: &Punctuated<syn::Variant, Token![,]>,
) -> TokenStream {
    let mut implicit_idx: T = num_traits::zero();
    let mut used_codes = HashSet::new();
    let mut encode_match_arms = Vec::new();
    let mut decode_match_arms = Vec::new();

    let variants = variants
        .into_iter()
        .map(|variant| {
            let attrs = VariantAttributes::<T>::parse(&variant.attrs);
            if let Some(code) = &attrs.code {
                let set = code.value.iter().copied().collect();
                for v in used_codes.intersection(&set) {
                    emit_error!(code.span, "The code `{}` is specified multiple times", v)
                }
                used_codes.extend(set);
            }
            (variant, attrs)
        })
        .collect::<Vec<_>>();

    proc_macro_error::abort_if_dirty();

    for (variant, variant_attrs) in variants {
        let name = &variant.ident;

        let fields = match &variant.fields {
            syn::Fields::Named(v) => Some(Fields::Named(
                v.named
                    .iter()
                    .map(|field| NamedField {
                        ident: field.ident.clone().unwrap(),
                        type_: field.ty.clone(),
                        attrs: FieldAttributes::parse(&field.attrs),
                    })
                    .collect::<Vec<_>>(),
            )),
            syn::Fields::Unnamed(v) => Some(Fields::Unnamed(
                v.unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, field)| UnnamedField {
                        index: i,
                        type_: field.ty.clone(),
                        attrs: FieldAttributes::parse(&field.attrs),
                    })
                    .collect::<Vec<_>>(),
            )),
            syn::Fields::Unit => None,
        };

        let combinations: Vec<_> = match &fields {
            Some(Fields::Named(fields)) => fields
                .iter()
                .map(patterns)
                .multi_cartesian_product()
                .map(|patterns| named_combination(name, patterns))
                .collect(),
            Some(Fields::Unnamed(fields)) => fields
                .iter()
                .map(patterns)
                .multi_cartesian_product()
                .map(|patterns| unnamed_combination(name, patterns))
                .collect(),
            None => vec![Combination {
                match_pattern: quote! { Self::#name },
                encode_fields: Vec::new(),
                build_self: quote! { Self::#name },
            }],
        };

        let mut iter = match variant_attrs.code {
            Some(SpannedValue { value, span }) => {
                if combinations.len() != value.len() {
                    abort!(
                        span,
                        "Invalid number of codes specified (expected `{}` but found `{}`)",
                        combinations.len(),
                        value.len()
                    )
                }
                Box::new(value.into_iter()) as Box<dyn Iterator<Item = T>>
            }
            None => Box::new(std::iter::from_fn(|| {
                let v = num_iter::range(implicit_idx, T::max_value())
                    .find(|v| !used_codes.contains(v))
                    .unwrap_or_else(|| {
                        abort!(
                            variant.ident.span(),
                            "No more available codes for this variant";
                            help = "Consider using a bigger instruction type"
                        )
                    });
                implicit_idx = v + T::one();
                Some(v)
            })) as Box<dyn Iterator<Item = T>>,
        };

        for combination in combinations {
            let i = iter.next().unwrap();
            let i_literal = i.to_literal();

            let match_pattern = &combination.match_pattern;
            let encode_fields = &combination.encode_fields;
            let type_ident = syn::Ident::new(std::any::type_name::<T>(), Span::call_site());
            let method_ident = syn::Ident::new(&format!("put_{}", type_ident), Span::call_site());
            encode_match_arms.push(quote! { #match_pattern => {
                buf.#method_ident(#i_literal);
                #( #encode_fields )*
            } });

            let build_self = &combination.build_self;
            decode_match_arms.push(quote! {
                #i_literal => #build_self,
            });
        }
    }

    let num_bytes = T::BYTES;
    let instruction_type_ident = syn::Ident::new(std::any::type_name::<T>(), Span::call_site());
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    quote! {
        impl #impl_generics ::bytecoding::Bytecode for #ident #type_generics #where_clause {
            fn encode<B: ::bytecoding::bytes::BufMut>(&self, buf: &mut B) {
                match self {
                    #( #encode_match_arms )*
                }
            }

            fn decode<B: ::bytecoding::bytes::Buf>(
                buf: &mut B,
            ) -> ::core::result::Result<Self, ::bytecoding::DecodeError> {
                if buf.remaining() < #num_bytes {
                    return ::core::result::Result::Err(
                        ::bytecoding::DecodeError::UnexpectedEnd
                    );
                }
                let mut be_bytes = [0; #num_bytes];
                buf.copy_to_slice(&mut be_bytes);
                let value = #instruction_type_ident::from_be_bytes(be_bytes);
                ::core::result::Result::Ok(match value {
                    #( #decode_match_arms )*
                    _ => return ::core::result::Result::Err(
                        ::bytecoding::DecodeError::InvalidValue(Box::new(be_bytes))
                    ),
                })
            }
        }
    }
}

fn named_combination(name: &syn::Ident, patterns: Vec<(Pattern, &NamedField)>) -> Combination {
    let build_self = {
        let fields = patterns.iter().map(|(pattern, field)| {
            let ident = &field.ident;
            match pattern {
                Pattern::Variable(_) => {
                    let type_ = &field.type_;
                    if field.attrs.skip {
                        quote! { #ident: <#type_ as ::core::default::Default>::default() }
                    } else {
                        quote! { #ident: <#type_ as ::bytecoding::Bytecode>::decode(buf)? }
                    }
                }
                Pattern::Value(pattern) => quote! { #ident: #pattern },
            }
        });
        quote! { Self::#name { #( #fields ),* } }
    };
    let field_patterns = patterns.iter().map(|(pattern, field)| {
        let ident = &field.ident;
        let pattern = match pattern {
            Pattern::Variable(v) | Pattern::Value(v) => v,
        };
        quote! { #ident: #pattern }
    });

    let match_pattern = quote! { Self::#name { #( #field_patterns ),* } };
    let encode_fields = patterns
        .iter()
        .filter(|(_, field)| !field.attrs.skip)
        .filter_map(|(pattern, field)| {
            let type_ = &field.type_;
            match pattern {
                Pattern::Variable(_) => {
                    let ident = field.binding_ident();
                    Some(quote! { <#type_ as ::bytecoding::Bytecode>::encode(#ident, buf); })
                }
                Pattern::Value(_) => None,
            }
        })
        .collect();

    Combination {
        match_pattern,
        encode_fields,
        build_self,
    }
}

fn unnamed_combination(name: &syn::Ident, patterns: Vec<(Pattern, &UnnamedField)>) -> Combination {
    let build_self = {
        let fields = patterns.iter().map(|(pattern, field)| match pattern {
            Pattern::Variable(_) => {
                let type_ = &field.type_;
                if field.attrs.skip {
                    quote! { <#type_ as ::core::default::Default>::default() }
                } else {
                    quote! { <#type_ as ::bytecoding::Bytecode>::decode(buf)? }
                }
            }
            Pattern::Value(pattern) => pattern.clone(),
        });
        quote! { Self::#name ( #( #fields ),* ) }
    };
    let field_patterns = patterns.iter().map(|(pattern, _)| match pattern {
        Pattern::Variable(v) | Pattern::Value(v) => v,
    });

    let match_pattern = quote! { Self::#name ( #( #field_patterns ),* ) };
    let encode_fields = patterns
        .iter()
        .filter(|(_, field)| !field.attrs.skip)
        .filter_map(|(pattern, field)| {
            let type_ = &field.type_;
            match pattern {
                Pattern::Variable(_) => {
                    let ident = field.binding_ident();
                    Some(quote! { <#type_ as ::bytecoding::Bytecode>::encode(#ident, buf); })
                }
                Pattern::Value(_) => None,
            }
        })
        .collect();

    Combination {
        match_pattern,
        encode_fields,
        build_self,
    }
}

trait FieldTrait {
    fn binding_ident(&self) -> syn::Ident;
    fn type_(&self) -> &syn::Type;
    fn attrs(&self) -> &FieldAttributes;
}

impl FieldTrait for NamedField {
    fn binding_ident(&self) -> syn::Ident {
        NamedField::binding_ident(self)
    }

    fn type_(&self) -> &syn::Type {
        &self.type_
    }

    fn attrs(&self) -> &FieldAttributes {
        &self.attrs
    }
}

impl FieldTrait for UnnamedField {
    fn binding_ident(&self) -> syn::Ident {
        UnnamedField::binding_ident(self)
    }

    fn type_(&self) -> &syn::Type {
        &self.type_
    }

    fn attrs(&self) -> &FieldAttributes {
        &self.attrs
    }
}

fn patterns<T: FieldTrait>(field: &T) -> Vec<(Pattern, &T)> {
    let mut patterns = Vec::new();
    if let Some(v) = &field.attrs().flatten_all {
        if field.attrs().flatten.is_some() {
            abort!(
                v.span,
                "Only one attribute of `flatten` and `flatten_all` can be used for the same field"
            );
        }
        patterns.extend(
            v.value
                .iter()
                .map(|pattern| (Pattern::Value(pattern.to_token_stream()), field)),
        );
        return patterns;
    }
    if let Some(v) = &field.attrs().flatten {
        patterns.extend(
            v.value
                .iter()
                .map(|pattern| (Pattern::Value(pattern.to_token_stream()), field)),
        );
    }
    patterns.push((
        Pattern::Variable(field.binding_ident().to_token_stream()),
        field,
    ));
    patterns
}
