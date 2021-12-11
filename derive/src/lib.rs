extern crate proc_macro;

#[macro_use]
mod shared;

mod serde_bin;
use crate::serde_bin::*;

mod serde_ron;
use crate::serde_ron::*;

mod serde_json;

mod parse;

use crate::serde_json::*;

#[proc_macro_derive(SerBin, attributes(nserde))]
pub fn derive_ser_bin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_ser_bin_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_ser_bin_struct(struct_),
        parse::Data::Struct(struct_) => derive_ser_bin_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_ser_bin_enum(enum_),
        _ => unimplemented!("Only structs and enums are supported"),
    };

    ts
}

#[proc_macro_derive(DeBin, attributes(nserde))]
pub fn derive_de_bin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_de_bin_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_de_bin_struct(struct_),
        parse::Data::Struct(struct_) => derive_de_bin_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_de_bin_enum(enum_),

        _ => unimplemented!("Only structs and enums are supported"),
    };

    ts
}

#[proc_macro_derive(SerRon, attributes(nserde))]
pub fn derive_ser_ron(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_ser_ron_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_ser_ron_struct(struct_),
        parse::Data::Struct(struct_) => derive_ser_ron_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_ser_ron_enum(enum_),
        _ => unimplemented!("Only structs and enums are supported"),
    };

    ts
}

#[proc_macro_derive(DeRon, attributes(nserde))]
pub fn derive_de_ron(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_de_ron_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_de_ron_struct(struct_),
        parse::Data::Struct(struct_) => derive_de_ron_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_de_ron_enum(enum_),
        _ => unimplemented!("Only structs and enums are supported"),
    };

    ts
}

/*
#[proc_macro_derive(SerJson, attributes(nserde))]
pub fn derive_ser_json(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_ser_json_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_ser_json_struct(struct_),
        parse::Data::Struct(struct_) => derive_ser_json_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_ser_json_enum(enum_),
        _ => unimplemented!(""),
    };

    ts
}

*/
use quote::{quote,format_ident};
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, parse_quote,Block,Data, DataStruct, DeriveInput, Fields, LitStr, Token};
use proc_macro::TokenStream;
use syn::{GenericArgument, Path, PathArguments, PathSegment};

fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
    match *ty {
        syn::Type::Path(ref typepath) if typepath.qself.is_none() => Some(&typepath.path),
        _ => None,
    }
}

// TODO store (with lazy static) the vec of string
// TODO maybe optimization, reverse the order of segments
fn extract_option_segment(path: &Path) -> Option<&PathSegment> {
    let idents_of_path = path
        .segments
        .iter()
        .into_iter()
        .fold(String::new(), |mut acc, v| {
            acc.push_str(&v.ident.to_string());
            acc.push('|');
            acc
        });
    vec!["Option|", "std|option|Option|", "core|option|Option|"]
        .into_iter()
        .find(|s| &idents_of_path == *s)
        .and_then(|_| path.segments.last())
}

#[proc_macro_derive(SerJson, attributes(nserde))]
pub fn derive_ser_json(input: TokenStream) -> TokenStream {

    let input = parse_macro_input!(input as DeriveInput);

    let fields = match &input.data {
        Data::Struct(DataStruct { fields: Fields::Named(fields), .. }) => &fields.named,
        _ => panic!("expected a struct with named fields"),
    };
    let field_name = fields.iter().map(|field| &field.ident );
    let field_name_string = fields.iter().map(|field| { 
            let paths = field.attrs.iter().map( |a| format!("{:?}",a.path) );
            format_ident!("{}",&field.ident.as_ref().unwrap()).to_string() 
    });
    let field_type_is_option = fields.iter().map(|field|  extract_type_path(&field.ty).and_then(|path| extract_option_segment(path))).map(|o| o.is_some());

    let struct_name = &input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut s_code = String::new();

    
    //s_code.push_str("{");

    for ((fname, fname_string), ftype_is_option) in field_name.into_iter().zip(field_name_string).zip(field_type_is_option) {

        if ftype_is_option {
            s_code.push_str(&format!("\nif self.{}.is_some(){{",fname_string));

            s_code.push_str( &format!( "\nif first_field_was_serialized {{ s.conl(); }};
                first_field_was_serialized = true;
                s.field(d+1, \"{}\");
                self.{}.ser_json(d+1, s);",fname_string,fname_string
                ));
            s_code.push_str("}");

        }else {
            s_code.push_str( &format!( "\nif first_field_was_serialized {{ s.conl(); }};
                first_field_was_serialized = true;
                s.field(d+1, \"{}\");
                self.{}.ser_json(d+1, s);",fname_string,fname_string
            ));
        }
        
    }

    
    //s_code.push_str("}");

    //unimplemented!("{}",&s_code);


    let s_impl_generics = (quote!{#impl_generics}).to_string();
    let s_struct_name = (quote!{#struct_name}).to_string();
    let s_ty_generics = (quote!{#ty_generics}).to_string();
    let s_where_clause = (quote!{#where_clause}).to_string();


    s_code  = format!("impl {} authcomp::ToJSON for {} {} {} {{
            
        fn ser_json (&self, d: usize, s: &mut authcomp::JSONState) {{

            let mut first_field_was_serialized  = false;

            s.st_pre();
        
            {}
           
            s.st_post(d);
        }}
    }}",s_impl_generics,s_struct_name,s_ty_generics,s_where_clause,s_code);

    let code : TokenStream = s_code.parse().unwrap();

    code

    /*
    unimplemented!("{}",(quote! {
        impl #impl_generics authcomp::ToJSON for #struct_name #ty_generics #where_clause {
            
            fn ser_json (&self, d: usize, s: &mut authcomp::JSONState) {

                let mut first_field_was_serialized  = false;

                s.st_pre();
            
                #s_code
               
                s.st_post(d);
            }
        }

    }).to_string());
    TokenStream::from(quote! {})
    */

    /*
    TokenStream::from(quote! {
        // Preserve the input struct unchanged in the output.
        //#input

        impl #struct_name {
            fn route() {

                // The following repetition expands to, for example:
                //
                //    let id: u32 = Default::default();
                //    let car_name: String = Default::default();
                #(
                    let #field_name: #field_type = Default::default();
                )*
            }
        }
    })
    */
}

#[proc_macro_derive(DeJson, attributes(nserde))]
pub fn derive_de_json(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_de_json_proxy(&proxy, &input.name());
    }
    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_de_json_struct(struct_),
        parse::Data::Struct(struct_) => derive_de_json_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_de_json_enum(enum_),
        parse::Data::Union(_) => unimplemented!("Unions are not supported"),
    };

    ts
}
