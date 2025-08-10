use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name, span = Span::call_site());
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let (struct_ty, members, types) = match &input.data {
        syn::Data::Struct(data_struct) => (
            match &data_struct.fields {
                syn::Fields::Named(_) => StructType::Named,
                syn::Fields::Unnamed(_) => StructType::Unnamed,
                syn::Fields::Unit => StructType::Union,
            },
            Vec::from_iter(data_struct.fields.members()),
            Vec::from_iter(data_struct.fields.iter().map(|f| f.ty.clone())),
        ),
        syn::Data::Enum(_) => std::panic!("expected struct, but found enum"),
        syn::Data::Union(_) => std::panic!("expected struct, but found union"),
    };

    let builder_definition = {
        let builder_field = members
            .iter()
            .zip(types.iter())
            .map(|(member, ty)| match member {
                syn::Member::Named(ident) => quote! { #ident: std::option::Option<#ty> },
                syn::Member::Unnamed(_) => quote! { std::option::Option<#ty> },
            });
        quote! {
            pub struct #builder_name #ty_generics #where_clause {
                #( #builder_field, )*
            }
        }
    };

    let builder_setter_impl = {
        let setter = members
            .iter()
            .zip(types.iter())
            .map(|(member, ty)| match member {
                syn::Member::Named(ident) => quote! {
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                },
                syn::Member::Unnamed(index) => {
                    let field_i = format_ident!("field_{}", index);
                    quote! {
                        pub fn #field_i(&mut self, #field_i: #ty) -> &mut Self {
                            self.#index = Some(#field_i);
                            self
                        }
                    }
                }
            });
        quote! {
            impl #impl_generics #builder_name #ty_generics #where_clause {
                #( #setter )*
            }
        }
    };

    let builder_build_impl = {
        let try_set_field = members.iter().map(|member| match member {
            syn::Member::Named(ident) => quote! {
                #ident: self.#ident.clone().ok_or("")?
            },
            syn::Member::Unnamed(index) => quote! {
                self.#index.clone().ok_or("")?
            },
        });

        match struct_ty {
            StructType::Named | StructType::Union => quote! {
                impl #impl_generics #builder_name #ty_generics #where_clause {
                    pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                        Ok(#name { #( #try_set_field, )* })
                    }
                }
            },
            StructType::Unnamed => quote! {
                impl #impl_generics #builder_name #ty_generics #where_clause {
                    pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                        Ok(#name ( #( #try_set_field, )* ))
                    }
                }
            },
        }
    };

    let original_builder_impl = {
        let builder_new = members.iter().map(|member| match member {
            syn::Member::Named(ident) => quote! { #ident: None },
            syn::Member::Unnamed(_) => quote! { None },
        });
        match struct_ty {
            StructType::Named | StructType::Union => quote! {
                impl #impl_generics #name #ty_generics #where_clause {
                    pub fn builder() -> #builder_name {
                        #builder_name { #( #builder_new, )* }
                    }
                }
            },
            StructType::Unnamed => quote! {
                impl #impl_generics #name #ty_generics #where_clause {
                    pub fn builder() -> #builder_name {
                        #builder_name ( #( #builder_new, )* )
                    }
                }
            },
        }
    };

    let extended = quote! {
        #builder_definition
        #builder_setter_impl
        #builder_build_impl

        #original_builder_impl
    };

    TokenStream::from(extended)
}

enum StructType {
    Named,
    Unnamed,
    Union,
}

fn is_option_ty(ty: &syn::Type) -> bool {
    let candidates: [syn::TypePath; 5] = [
        syn::parse_str("Option").unwrap(),
        syn::parse_str("std::option::Option").unwrap(),
        syn::parse_str("::std::option::Option").unwrap(),
        syn::parse_str("core::option::Option").unwrap(),
        syn::parse_str("::core::option::Option").unwrap(),
    ];

    if let syn::Type::Path(type_path) = ty {
        candidates.contains(type_path)
    } else {
        false
    }
}
