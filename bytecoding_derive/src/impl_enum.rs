use crate::{
    buf_put_method_call, FieldAttributes, Fields, InstructionType, NamedField, SpannedValue,
    UnnamedField,
};
use itertools::Itertools;
use proc_macro2::TokenStream;
use proc_macro_error::{abort, abort_call_site};
use quote::{quote, ToTokens};
use syn::{punctuated::Punctuated, Token};

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

pub(crate) fn impl_bytecode(
    ident: &syn::Ident,
    generics: &syn::Generics,
    instruction_type: SpannedValue<InstructionType>,
    variants: &Punctuated<syn::Variant, Token![,]>,
) -> TokenStream {
    let mut i: usize = 0;
    let mut encode_match_arms = Vec::new();
    let mut decode_match_arms = Vec::new();
    for variant in variants {
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

        for combination in combinations {
            let match_pattern = combination.match_pattern;
            let encode_fields = combination.encode_fields;
            let buf_put_method_call = buf_put_method_call(instruction_type.value, i);
            encode_match_arms.push(quote! { #match_pattern => {
                #buf_put_method_call;
                #( #encode_fields )*
            } });

            let build_self = combination.build_self;
            let i_casted = instruction_type.value.casted_literal(i).unwrap_or_else(|| {
                abort_call_site!(
                    "There are more instruction codes than can fit in the type `{}`",
                    instruction_type.value;
                    help = "Consider using a bigger instruction type"
                )
            });
            decode_match_arms.push(quote! {
                #i_casted => #build_self,
            });

            i += 1;
        }
    }

    let num_bytes = instruction_type.value.num_bytes();
    let instruction_type_ident = instruction_type.value.type_ident();
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
            Pattern::Variable(v) => v,
            Pattern::Value(v) => v,
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
        Pattern::Variable(v) => v,
        Pattern::Value(v) => v,
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
