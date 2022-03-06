use crate::{FieldAttributes, Fields, NamedField, UnnamedField};
use proc_macro2::{Literal, TokenStream};
use proc_macro_error::{abort, emit_error};
use quote::{quote, ToTokens};

pub(crate) fn impl_bytecode(
    ident: &syn::Ident,
    generics: &syn::Generics,
    fields: &syn::Fields,
) -> TokenStream {
    let fields = match fields {
        syn::Fields::Named(v) => Fields::Named(
            v.named
                .iter()
                .map(|field| NamedField {
                    ident: field.ident.clone().unwrap(),
                    type_: field.ty.clone(),
                    attrs: FieldAttributes::parse(&field.attrs),
                })
                .collect(),
        ),
        syn::Fields::Unnamed(v) => Fields::Unnamed(
            v.unnamed
                .iter()
                .enumerate()
                .map(|(i, field)| UnnamedField {
                    index: i,
                    type_: field.ty.clone(),
                    attrs: FieldAttributes::parse(&field.attrs),
                })
                .collect::<Vec<_>>(),
        ),
        syn::Fields::Unit => {
            abort!(
                ident.span(),
                "The `Bytecode` derive macro does not support unit structs"
            )
        }
    };
    if match &fields {
        Fields::Named(fields) => !fields.iter().any(|field| !field.attrs.skip),
        Fields::Unnamed(fields) => !fields.iter().any(|field| !field.attrs.skip),
    } {
        abort!(
            ident.span(),
            "The `Bytecode` derive macro requires at least one field"
        );
    }
    let check_attrs = |attrs: &FieldAttributes| {
        if let Some(v) = &attrs.flatten {
            emit_error!(v.span, "The `flatten` attribute is not allowed for structs");
        }
        if let Some(v) = &attrs.flatten_all {
            emit_error!(
                v.span,
                "The `flatten_all` attribute is not allowed for structs"
            );
        }
    };
    match &fields {
        Fields::Named(v) => v.iter().for_each(|field| check_attrs(&field.attrs)),
        Fields::Unnamed(v) => v.iter().for_each(|field| check_attrs(&field.attrs)),
    };

    let encode_fields: Vec<_> = match &fields {
        Fields::Named(fields) => fields
            .iter()
            .filter(|field| !field.attrs.skip)
            .map(|field| {
                let ident = &field.ident;
                let type_ = &field.type_;
                quote! { <#type_ as ::bytecoding::Bytecode>::encode(&self.#ident, buf); }
            })
            .collect(),
        Fields::Unnamed(fields) => fields
            .iter()
            .filter(|field| !field.attrs.skip)
            .map(|field| {
                let ident = Literal::usize_unsuffixed(field.index).to_token_stream();
                let type_ = &field.type_;
                quote! { <#type_ as ::bytecoding::Bytecode>::encode(&self.#ident, buf); }
            })
            .collect(),
    };

    let build_self = match &fields {
        Fields::Named(fields) => {
            let build_fields = fields.iter().map(|field| {
                let ident = &field.ident;
                let type_ = &field.type_;
                if field.attrs.skip {
                    quote! { #ident: <#type_ as ::core::default::Default>::default() }
                } else {
                    quote! { #ident: <#type_ as ::bytecoding::Bytecode>::decode(buf)? }
                }
            });
            quote! { Self { #( #build_fields ),* } }
        }
        Fields::Unnamed(fields) => {
            let build_fields = fields.iter().map(|field| {
                let type_ = &field.type_;
                if field.attrs.skip {
                    quote! { <#type_ as ::core::default::Default>::default() }
                } else {
                    quote! { <#type_ as ::bytecoding::Bytecode>::decode(buf)? }
                }
            });
            quote! { Self ( #( #build_fields ),* ) }
        }
    };

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    quote! {
        impl #impl_generics ::bytecoding::Bytecode for #ident #type_generics #where_clause {
            fn encode<B: ::bytecoding::bytes::BufMut>(&self, buf: &mut B) {
                #( #encode_fields )*
            }

            fn decode<B: ::bytecoding::bytes::Buf>(
                buf: &mut B,
            ) -> ::core::result::Result<Self, ::bytecoding::DecodeError> {
                ::core::result::Result::Ok(#build_self)
            }

        }
    }
}
