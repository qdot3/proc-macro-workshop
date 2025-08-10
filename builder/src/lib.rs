use itertools::izip;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name, span = Span::call_site());
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let (struct_ty, members, types, attrs) = match &input.data {
        syn::Data::Struct(data_struct) => (
            match &data_struct.fields {
                syn::Fields::Named(_) => StructType::Named,
                syn::Fields::Unnamed(_) => StructType::Unnamed,
                syn::Fields::Unit => StructType::Union,
            },
            Vec::from_iter(data_struct.fields.members()),
            Vec::from_iter(data_struct.fields.iter().map(|f| f.ty.clone())),
            Vec::from_iter(data_struct.fields.iter().map(|f| f.attrs.clone())),
        ),
        syn::Data::Enum(_) => std::panic!("expected struct, but found enum"),
        syn::Data::Union(_) => std::panic!("expected struct, but found union"),
    };

    let builder_definition = {
        let builder_field = members.iter().zip(types.iter()).map(|(member, ty)| {
            let field_ty = match extract_option_ty(ty) {
                Some(_) => quote! { #ty },
                None => quote! { std::option::Option<#ty> },
            };
            match member {
                syn::Member::Named(ident) => quote! { #ident: #field_ty },
                syn::Member::Unnamed(_) => quote! { #field_ty },
            }
        });
        quote! {
            pub struct #builder_name #ty_generics #where_clause {
                #( #builder_field, )*
            }
        }
    };

    // struct_field: T or Option<T> -> pub fn field(&mut self, filed: T) -> &mut Self;
    let builder_setter_impl = {
        let setter = members.iter().zip(types.iter()).map(|(member, ty)| {
            let ty = extract_option_ty(ty).unwrap_or(ty.clone());
            match member {
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
            }
        });
        quote! {
            impl #impl_generics #builder_name #ty_generics #where_clause {
                #( #setter )*
            }
        }
    };

    // #[builder(each = arg)] Vec<T> -> pub fn arg(&mut self, arg: T) -> &mut Self;
    let builder_each_setter_impl = {
        let each_setter = izip!(&members, &types, &attrs).flat_map(|(member, ty, attrs)| {
            let mut each_setter = std::vec::Vec::new();
            if let Some(ty) = extract_vec_ty(ty) {
                for attr in attrs {
                    if attr.path().is_ident("builder") {
                        if let Err(e) = attr.parse_nested_meta(|meta| {
                            if meta.path.is_ident("each") {
                                let arg: syn::LitStr = meta.value()?.parse()?;
                                let arg = syn::Ident::new(&arg.value(), Span::call_site());
                                match member {
                                    syn::Member::Named(ident) if ident != &arg => {
                                        each_setter.push(quote! {
                                            pub fn #arg(&mut self, #arg: #ty) -> &mut Self {
                                                self.#ident.get_or_insert(std::vec::Vec::new()).push(#arg);
                                                self
                                            }
                                        })} ,
                                    syn::Member::Unnamed(index) => each_setter.push(quote! {
                                        pub fn #arg(&mut self, #arg: #ty) -> &mut Self {
                                            self.#index.get_or_insert_default().push(#arg);
                                            self
                                        }
                                    }),
                                    _ => (),
                                };
                                Ok(())
                            } else {
                                Err(meta.error("expected #[builder(each = ...)]"))
                            }
                        }) {
                            each_setter.push(e.to_compile_error());
                        }
                    }
                }
            }
            each_setter
        });
        quote! {
            impl #impl_generics #builder_name #ty_generics #where_clause {
                #( #each_setter )*
            }
        }
    };

    let builder_build_impl = {
        let try_set_field = members
            .iter()
            .zip(types.iter())
            .map(|(member, ty)| match member {
                syn::Member::Named(ident) => {
                    if extract_option_ty(ty).is_some() {
                        quote! { #ident: self.#ident.clone() }
                    } else if extract_vec_ty(ty).is_some() {
                        quote! { #ident: self.#ident.clone().unwrap_or_default()}
                    } else {
                        quote! { #ident: self.#ident.clone().ok_or("")? }
                    }
                }
                syn::Member::Unnamed(index) => {
                    if extract_option_ty(ty).is_some() {
                        quote! { self.#index.clone() }
                    } else if extract_vec_ty(ty).is_some() {
                        quote! { self.#index.clone().unwrap_or_default() }
                    } else {
                        quote! { self.#index.clone().ok_or("")? }
                    }
                }
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
        #builder_each_setter_impl
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

fn extract_option_ty(ty: &syn::Type) -> Option<syn::Type> {
    let path = if let syn::Type::Path(syn::TypePath { qself, path }) = ty {
        if qself.is_some() {
            return None;
        }
        path.clone()
    } else {
        return None;
    };

    let candidates = Vec::from_iter(
        [
            "Option",
            "std::option::Option",
            // "::std::option::Option",
            "core::option::Option",
            // "::core::option::Option",
        ]
        .into_iter()
        .map(|s| syn::parse_str::<syn::TypePath>(s).unwrap().path),
    );
    for candidate in candidates {
        let len = candidate.segments.len();
        if path.segments.len() == len
            && (0..len - 1).all(|i| candidate.segments[i] == path.segments[i])
            && candidate.segments[len - 1].ident == path.segments[len - 1].ident
        {
            match &path.segments[len - 1].arguments {
                syn::PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                    if let Some(syn::GenericArgument::Type(ty)) =
                        angle_bracketed_generic_arguments.args.first()
                    {
                        return Some(ty.clone());
                    }
                }
                _ => continue,
            }
        }
    }

    None
}

#[test]
fn test_extract_option_ty() {
    let ty = syn::parse_str::<syn::Type>("std::option::Option<String>").unwrap();
    assert_eq!(
        extract_option_ty(&ty).unwrap(),
        syn::parse_str("String").unwrap()
    )
}

fn extract_vec_ty(ty: &syn::Type) -> Option<syn::Type> {
    let path = if let syn::Type::Path(syn::TypePath { qself, path }) = ty {
        if qself.is_some() {
            return None;
        }
        path.clone()
    } else {
        return None;
    };

    let candidates = Vec::from_iter(
        ["Vec", "std::vec::Vec"]
            .into_iter()
            .map(|s| syn::parse_str::<syn::TypePath>(s).unwrap().path),
    );
    for candidate in candidates {
        let len = candidate.segments.len();
        if path.segments.len() == len
            && (0..len - 1).all(|i| candidate.segments[i] == path.segments[i])
            && candidate.segments[len - 1].ident == path.segments[len - 1].ident
        {
            match &path.segments[len - 1].arguments {
                syn::PathArguments::AngleBracketed(angle_bracketed_generic_arguments) => {
                    if let Some(syn::GenericArgument::Type(ty)) =
                        angle_bracketed_generic_arguments.args.first()
                    {
                        return Some(ty.clone());
                    }
                }
                _ => continue,
            }
        }
    }

    None
}

#[test]
fn test_extract_vec_ty() {
    let ty = syn::parse_str::<syn::Type>("Vec<String>").unwrap();
    assert_eq!(
        extract_vec_ty(&ty).unwrap(),
        syn::parse_str("String").unwrap()
    )
}
