use crate::expression_tree::{BuiltinFunction, EasingCurve, MinMaxOp, OperatorClass};
use crate::langtype::{Enumeration, EnumerationValue, NativeClass, Type};
use crate::layout::Orientation;
use crate::llr::{
    self, EvaluationContext as llr_EvaluationContext, ParentCtx as llr_ParentCtx,
    TypeResolutionContext as _,
};
use crate::object_tree::Document;
use crate::CompilerConfiguration;
use itertools::Either;
use smol_str::{format_smolstr, SmolStr};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;
use std::io::BufWriter;
use std::num::NonZeroUsize;

/// The configuration for the C# code generator
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Config {
    pub namespace: Option<String>,
    pub output_directory: std::path::PathBuf,
    pub assembly_name: Option<String>,
}


// Check if word is one of C# keywords
fn is_csharp_keyword(word: &str) -> bool {
    matches!(
        word,
        "abstract" | "as" | "base" | "bool" | "break" | "byte" | "case" | "catch" | "char"
            | "checked" | "class" | "const" | "continue" | "decimal" | "default" | "delegate"
            | "do" | "double" | "else" | "enum" | "event" | "explicit" | "extern" | "false"
            | "finally" | "fixed" | "float" | "for" | "foreach" | "goto" | "if" | "implicit"
            | "in" | "int" | "interface" | "internal" | "is" | "lock" | "long" | "namespace"
            | "new" | "null" | "object" | "operator" | "out" | "override" | "params" | "private"
            | "protected" | "public" | "readonly" | "ref" | "return" | "sbyte" | "sealed"
            | "short" | "sizeof" | "stackalloc" | "static" | "string" | "struct" | "switch"
            | "this" | "throw" | "true" | "try" | "typeof" | "uint" | "ulong" | "unchecked"
            | "unsafe" | "ushort" | "using" | "virtual" | "void" | "volatile" | "while"
    )
}

pub fn ident(ident: &str) -> SmolStr {
    let mut new_ident = SmolStr::from(ident);
    if ident.contains('-') {
        new_ident = ident.replace("-", "_").into();
    }
    if is_csharp_keyword(&new_ident) {
        new_ident = format_smolstr!("@{}", new_ident);
    }
    new_ident
}

pub fn pascal_case(ident: &str) -> SmolStr {
    let clean_ident = ident.replace("-", "_");
    let mut result = String::new();
    let mut capitalize_next = true;
    
    for c in clean_ident.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    
    if is_csharp_keyword(&result) {
        format_smolstr!("@{}", result)
    } else {
        result.into()
    }
}

/// This module contains data structures that help represent C# code.
pub mod csharp_ast {
    use std::fmt::Write;
use std::fmt::{Display, Error, Formatter};
    use smol_str::{format_smolstr, SmolStr};

    /// A full C# file
    #[derive(Default, Debug)]
    pub struct File {
        pub usings: Vec<SmolStr>,
        pub namespace: Option<String>,
        pub declarations: Vec<Declaration>,
    }

    impl Display for File {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            writeln!(f, "// This file is auto-generated")?;
            writeln!(f, "#nullable enable")?;
            
            for using in &self.usings {
                writeln!(f, "using {};", using)?;
            }
            
            if !self.usings.is_empty() {
                writeln!(f)?;
            }

            if let Some(namespace) = &self.namespace {
                writeln!(f, "namespace {}", namespace)?;
                writeln!(f, "{{")?;
                for d in &self.declarations {
                    write!(f, "{}", d)?;
                }
                writeln!(f, "}}")?;
            } else {
                for d in &self.declarations {
                    write!(f, "{}", d)?;
                }
            }

            Ok(())
        }
    }

    /// Declarations (top level, or within a class)
    #[derive(Debug)]
    pub enum Declaration {
        Class(Class),
        Enum(Enum),
        Method(Method),
        Property(Property),
        Field(Field),
    }

    impl Display for Declaration {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            match self {
                Declaration::Class(c) => write!(f, "{}", c),
                Declaration::Enum(e) => write!(f, "{}", e),
                Declaration::Method(m) => write!(f, "{}", m),
                Declaration::Property(p) => write!(f, "{}", p),
                Declaration::Field(field) => write!(f, "{}", field),
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub enum Visibility {
        Public,
        Private,
        Protected,
        Internal,
    }

    impl Display for Visibility {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            match self {
                Visibility::Public => write!(f, "public"),
                Visibility::Private => write!(f, "private"),
                Visibility::Protected => write!(f, "protected"),
                Visibility::Internal => write!(f, "internal"),
            }
        }
    }

    #[derive(Default, Debug)]
    pub struct Class {
        pub name: SmolStr,
        pub visibility: Option<Visibility>,
        pub is_static: bool,
        pub is_partial: bool,
        pub base_class: Option<SmolStr>,
        pub interfaces: Vec<SmolStr>,
        pub members: Vec<Declaration>,
    }

    impl Display for Class {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            if let Some(vis) = self.visibility {
                write!(f, "{} ", vis)?;
            }
            if self.is_static {
                write!(f, "static ")?;
            }
            if self.is_partial {
                write!(f, "partial ")?;
            }
            write!(f, "class {}", self.name)?;
            
            let mut inheritance = Vec::new();
            if let Some(base) = &self.base_class {
                inheritance.push(base.as_str());
            }
            inheritance.extend(self.interfaces.iter().map(|i| i.as_str()));
            
            if !inheritance.is_empty() {
                write!(f, " : {}", inheritance.join(", "))?;
            }
            
            writeln!(f)?;
            writeln!(f, "{{")?;
            
            for member in &self.members {
                write!(f, "    {}", member)?;
            }
            
            writeln!(f, "}}")?;
            Ok(())
        }
    }

    #[derive(Default, Debug)]
    pub struct Enum {
        pub name: SmolStr,
        pub visibility: Option<Visibility>,
        pub values: Vec<SmolStr>,
    }

    impl Display for Enum {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            if let Some(vis) = self.visibility {
                write!(f, "{} ", vis)?;
            }
            writeln!(f, "enum {}", self.name)?;
            writeln!(f, "{{")?;
            for (i, value) in self.values.iter().enumerate() {
                if i == self.values.len() - 1 {
                    writeln!(f, "    {}", value)?;
                } else {
                    writeln!(f, "    {},", value)?;
                }
            }
            writeln!(f, "}}")?;
            Ok(())
        }
    }

    #[derive(Default, Debug)]
    pub struct Method {
        pub name: SmolStr,
        pub visibility: Option<Visibility>,
        pub is_static: bool,
        pub is_virtual: bool,
        pub is_override: bool,
        pub return_type: SmolStr,
        pub parameters: Vec<Parameter>,
        pub body: Option<Vec<String>>,
    }

    impl Display for Method {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            if let Some(vis) = self.visibility {
                write!(f, "{} ", vis)?;
            }
            if self.is_static {
                write!(f, "static ")?;
            }
            if self.is_virtual {
                write!(f, "virtual ")?;
            }
            if self.is_override {
                write!(f, "override ")?;
            }
            
            write!(f, "{} {}(", self.return_type, self.name)?;
            for (i, param) in self.parameters.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", param)?;
            }
            write!(f, ")")?;
            
            if let Some(body) = &self.body {
                writeln!(f)?;
                writeln!(f, "    {{")?;
                for statement in body {
                    writeln!(f, "        {}", statement)?;
                }
                writeln!(f, "    }}")?;
            } else {
                writeln!(f, ";")?;
            }
            
            Ok(())
        }
    }

    #[derive(Default, Debug)]
    pub struct Property {
        pub name: SmolStr,
        pub visibility: Option<Visibility>,
        pub is_static: bool,
        pub property_type: SmolStr,
        pub getter: Option<Vec<String>>,
        pub setter: Option<Vec<String>>,
        pub is_auto: bool,
    }

    impl Display for Property {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            if let Some(vis) = self.visibility {
                write!(f, "{} ", vis)?;
            }
            if self.is_static {
                write!(f, "static ")?;
            }
            
            write!(f, "{} {}", self.property_type, self.name)?;
            
            if self.is_auto {
                writeln!(f, " {{ get; set; }}")?;
            } else {
                writeln!(f)?;
                writeln!(f, "    {{")?;
                
                if let Some(getter) = &self.getter {
                    writeln!(f, "        get")?;
                    writeln!(f, "        {{")?;
                    for statement in getter {
                        writeln!(f, "            {}", statement)?;
                    }
                    writeln!(f, "        }}")?;
                }
                
                if let Some(setter) = &self.setter {
                    writeln!(f, "        set")?;
                    writeln!(f, "        {{")?;
                    for statement in setter {
                        writeln!(f, "            {}", statement)?;
                    }
                    writeln!(f, "        }}")?;
                }
                
                writeln!(f, "    }}")?;
            }
            
            Ok(())
        }
    }

    #[derive(Default, Debug)]
    pub struct Field {
        pub name: SmolStr,
        pub visibility: Option<Visibility>,
        pub is_static: bool,
        pub is_readonly: bool,
        pub field_type: SmolStr,
        pub initializer: Option<String>,
    }

    impl Display for Field {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            if let Some(vis) = self.visibility {
                write!(f, "{} ", vis)?;
            }
            if self.is_static {
                write!(f, "static ")?;
            }
            if self.is_readonly {
                write!(f, "readonly ")?;
            }
            
            write!(f, "{} {}", self.field_type, self.name)?;
            
            if let Some(init) = &self.initializer {
                write!(f, " = {}", init)?;
            }
            
            writeln!(f, ";")?;
            Ok(())
        }
    }

    #[derive(Default, Debug)]
    pub struct Parameter {
        pub name: SmolStr,
        pub parameter_type: SmolStr,
        pub modifier: Option<ParameterModifier>,
    }

    impl Display for Parameter {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            if let Some(modifier) = &self.modifier {
                write!(f, "{} ", modifier)?;
            }
            write!(f, "{} {}", self.parameter_type, self.name)
        }
    }

    #[derive(Debug)]
    pub enum ParameterModifier {
        Ref,
        Out,
        In,
        Params,
    }

    impl Display for ParameterModifier {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
            match self {
                ParameterModifier::Ref => write!(f, "ref"),
                ParameterModifier::Out => write!(f, "out"),
                ParameterModifier::In => write!(f, "in"),
                ParameterModifier::Params => write!(f, "params"),
            }
        }
    }

    pub fn escape_string(str: &str) -> String {
        let mut result = String::with_capacity(str.len());
        for x in str.chars() {
            match x {
                '\n' => result.push_str("\\n"),
                '\\' => result.push_str("\\\\"),
                '\"' => result.push_str("\\\""),
                '\t' => result.push_str("\\t"),
                '\r' => result.push_str("\\r"),
                _ if !x.is_ascii() || (x as u32) < 32 => {
                    write!(result, "\\u{:04x}", x as u32).unwrap();
                }
                _ => result.push(x),
            }
        }
        result
    }
}

use csharp_ast::*;
use std::cell::Cell;

#[derive(Default)]
struct ConditionalUsings {
    system_collections_generic: Cell<bool>,
    system_linq: Cell<bool>,
    system_threading_tasks: Cell<bool>,
}

#[derive(Clone)]
struct CsharpGeneratorContext<'a> {
    global_access: String,
    conditional_usings: &'a ConditionalUsings,
}

type EvaluationContext<'a> = llr_EvaluationContext<'a, CsharpGeneratorContext<'a>>;
type ParentCtx<'a> = llr_ParentCtx<'a, CsharpGeneratorContext<'a>>;

impl Type {
    fn csharp_type(&self) -> Option<SmolStr> {
        match self {
            Type::Void => Some("void".into()),
            Type::Float32 => Some("float".into()),
            Type::Int32 => Some("int".into()),
            Type::String => Some("string".into()),
            Type::Color => Some("Slint.Color".into()),
            Type::Duration => Some("long".into()),
            Type::Angle => Some("float".into()),
            Type::PhysicalLength => Some("float".into()),
            Type::LogicalLength => Some("float".into()),
            Type::Rem => Some("float".into()),
            Type::Percent => Some("float".into()),
            Type::Bool => Some("bool".into()),
            Type::Struct(s) => match (&s.name, &s.node) {
                (Some(name), Some(_)) => Some(pascal_case(name)),
                (Some(name), None) => Some(format_smolstr!("Slint.{}", pascal_case(name))),
                _ => {
                    // Anonymous struct - use tuple syntax
                    let elem = s.fields.values().map(|v| v.csharp_type()).collect::<Option<Vec<_>>>()?;
                    Some(format_smolstr!("({})", elem.join(", ")))
                }
            },
            Type::Array(i) => {
                Some(format_smolstr!("IReadOnlyList<{}>", i.csharp_type()?))
            }
            Type::Image => Some("Slint.Image".into()),
            Type::Enumeration(enumeration) => {
                if enumeration.node.is_some() {
                    Some(pascal_case(&enumeration.name))
                } else {
                    Some(format_smolstr!("Slint.{}", pascal_case(&enumeration.name)))
                }
            }
            Type::Brush => Some("Slint.Brush".into()),
            Type::LayoutCache => Some("float[]".into()),
            Type::Easing => Some("Slint.EasingCurve".into()),
            Type::Callback(callback) => {
                let param_types = callback.args.iter().map(|t| t.csharp_type()).collect::<Option<Vec<_>>>()?;
                let return_type = callback.return_type.csharp_type()?;
                if return_type == "void" {
                    if param_types.is_empty() {
                        Some(format_smolstr!("Action"))
                    } else {
                        Some(format_smolstr!("Action<{}>", param_types.join(", ")))
                    }
                } else {
                    Some(format_smolstr!("Func<{}, {}>", param_types.join(", "), return_type))
                }
            }
            _ => None,
        }
    }
}

fn to_csharp_orientation(o: Orientation) -> &'static str {
    match o {
        Orientation::Horizontal => "Slint.Orientation.Horizontal",
        Orientation::Vertical => "Slint.Orientation.Vertical",
    }
}

/// Returns the text of the C# code produced by the given root component
pub fn generate(
    doc: &Document,
    config: Config,
    compiler_config: &CompilerConfiguration,
) -> std::io::Result<impl std::fmt::Display> {
    let mut file = generate_types(&doc.used_types.borrow().structs_and_enums, &config);

    let llr = llr::lower_to_item_tree::lower_to_item_tree(doc, compiler_config)?;

    if llr.public_components.is_empty() {
        return Ok(file);
    }

    let conditional_usings = ConditionalUsings::default();

    // Generate public components
    for component in &llr.public_components {
        generate_public_component(&mut file, &conditional_usings, component, &llr);
    }

    // Generate globals
    for (idx, global) in llr.globals.iter_enumerated() {
        if global.exported && global.must_generate() {
            generate_global(&mut file, &conditional_usings, idx, global, &llr);
        }
    }

    // Add conditional usings
    if conditional_usings.system_collections_generic.get() {
        file.usings.push("System.Collections.Generic".into());
    }
    if conditional_usings.system_linq.get() {
        file.usings.push("System.Linq".into());
    }
    if conditional_usings.system_threading_tasks.get() {
        file.usings.push("System.Threading.Tasks".into());
    }

    // Add required usings
    file.usings.insert(0, "System".into());
    file.usings.insert(1, "Slint.Inerop".into());

    Ok(file)
}

pub fn generate_types(used_types: &[Type], config: &Config) -> File {
    let mut file = File {
        namespace: config.namespace.clone(),
        ..Default::default()
    };

    for ty in used_types {
        match ty {
            Type::Struct(s) if s.name.is_some() && s.node.is_some() => {
                generate_struct(&mut file, s.name.as_ref().unwrap(), &s.fields);
            }
            Type::Enumeration(en) => {
                generate_enum(&mut file, en);
            }
            _ => (),
        }
    }

    file
}

fn generate_struct(file: &mut File, name: &str, fields: &BTreeMap<SmolStr, Type>) {
    let class_name = pascal_case(name);
    let mut properties = Vec::new();

    for (field_name, field_type) in fields {
        properties.push(Declaration::Property(Property {
            name: pascal_case(field_name),
            visibility: Some(Visibility::Public),
            property_type: field_type.csharp_type().unwrap_or_else(|| "object".into()),
            is_auto: true,
            ..Default::default()
        }));
    }

    file.declarations.push(Declaration::Class(Class {
        name: class_name,
        visibility: Some(Visibility::Public),
        members: properties,
        ..Default::default()
    }));
}

fn generate_enum(file: &mut File, en: &std::rc::Rc<Enumeration>) {
    let enum_name = pascal_case(&en.name);
    let values = (0..en.values.len())
        .map(|value| {
            pascal_case(&EnumerationValue { value, enumeration: en.clone() }.to_pascal_case())
        })
        .collect();

    file.declarations.push(Declaration::Enum(Enum {
        name: enum_name,
        visibility: Some(Visibility::Public),
        values,
    }));
}

fn generate_public_component(
    file: &mut File,
    conditional_usings: &ConditionalUsings,
    component: &llr::PublicComponent,
    unit: &llr::CompilationUnit,
) {
    let component_name = pascal_case(&component.name);
    let mut members = Vec::new();

    let ctx = EvaluationContext {
        compilation_unit: unit,
        current_sub_component: Some(component.item_tree.root),
        current_global: None,
        generator_state: CsharpGeneratorContext {
            global_access: "this.globals".to_string(),
            conditional_usings,
        },
        parent: None,
        argument_types: &[],
    };

    // Generate constructor
    members.push(Declaration::Method(Method {
        name: component_name.clone(),
        visibility: Some(Visibility::Public),
        return_type: "".into(), // Constructor has no return type
        body: Some(vec![
            "// Initialize component".to_string(),
            "this.Initialize();".to_string(),
        ]),
        ..Default::default()
    }));

    // Generate Show method
    members.push(Declaration::Method(Method {
        name: "Show".into(),
        visibility: Some(Visibility::Public),
        return_type: "void".into(),
        body: Some(vec![
            "Slint.Interop.Window.Show();".to_string(),
        ]),
        ..Default::default()
    }));

    // Generate Hide method
    members.push(Declaration::Method(Method {
        name: "Hide".into(),
        visibility: Some(Visibility::Public),
        return_type: "void".into(),
        body: Some(vec![
            "Slint.Interop.Window.Hide();".to_string(),
        ]),
        ..Default::default()
    }));

    // Generate Window property
    members.push(Declaration::Property(Property {
        name: "Window".into(),
        visibility: Some(Visibility::Public),
        property_type: "Slint.Interop.Window".into(),
        is_auto: true,
        ..Default::default()
    }));

    // Generate public API for properties
    generate_public_api_for_properties(&mut members, &component.public_properties, &ctx);

    file.declarations.push(Declaration::Class(Class {
        name: component_name,
        visibility: Some(Visibility::Public),
        base_class: Some("Slint.Interop.ComponentBase".into()),
        members,
        ..Default::default()
    }));
}

fn generate_public_api_for_properties(
    members: &mut Vec<Declaration>,
    public_properties: &llr::PublicProperties,
    ctx: &EvaluationContext,
) {
    for p in public_properties {
        let prop_name = pascal_case(&p.name);
        let csharp_type = p.ty.csharp_type().unwrap_or_else(|| "object".into());

        if let Type::Callback(_) = &p.ty {
            // Generate event
            members.push(Declaration::Field(Field {
                name: prop_name.clone(),
                visibility: Some(Visibility::Public),
                field_type: format_smolstr!("event {}", csharp_type),
                ..Default::default()
            }));
        } else {
            // Generate property
            members.push(Declaration::Property(Property {
                name: prop_name,
                visibility: Some(Visibility::Public),
                property_type: csharp_type,
                getter: Some(vec![
                    format!("return GetProperty<{}>();", p.ty.csharp_type().unwrap_or_else(|| "object".into())),
                ]),
                setter: if !p.read_only {
                    Some(vec![
                        "SetProperty(value);".to_string(),
                    ])
                } else {
                    None
                },
                ..Default::default()
            }));
        }
    }
}

fn generate_global(
    file: &mut File,
    conditional_usings: &ConditionalUsings,
    global_idx: llr::GlobalIdx,
    global: &llr::GlobalComponent,
    root: &llr::CompilationUnit,
) {
    let global_name = pascal_case(&global.name);
    let mut members = Vec::new();

    let ctx = EvaluationContext::new_global(
        root,
        global_idx,
        CsharpGeneratorContext {
            global_access: "this".to_string(),
            conditional_usings,
        },
    );

    // Generate properties for the global
    for property in global.properties.iter().filter(|p| p.use_count.get() > 0) {
        let prop_name = pascal_case(&property.name);
        let csharp_type = property.ty.csharp_type().unwrap_or_else(|| "object".into());

        members.push(Declaration::Property(Property {
            name: prop_name,
            visibility: Some(Visibility::Public),
            property_type: csharp_type,
            is_auto: true,
            ..Default::default()
        }));
    }

    // Generate public API
    generate_public_api_for_properties(&mut members, &global.public_properties, &ctx);

    file.declarations.push(Declaration::Class(Class {
        name: global_name,
        visibility: Some(Visibility::Public),
        is_static: true,
        members,
        ..Default::default()
    }));
}

fn compile_expression(expr: &llr::Expression, ctx: &EvaluationContext) -> String {
    use llr::Expression;
    match expr {
        Expression::StringLiteral(s) => {
            format!(r#""{}""#, escape_string(s.as_str()))
        }
        Expression::NumberLiteral(num) => {
            if !num.is_finite() {
                "0.0f".to_string()
            } else {
                format!("{}f", num)
            }
        }
        Expression::BoolLiteral(b) => {
            if *b { "true" } else { "false" }.to_string()
        }
        Expression::PropertyReference(nr) => {
            access_member(nr, ctx)
        }
        Expression::BuiltinFunctionCall { function, arguments } => {
            compile_builtin_function_call(function.clone(), arguments, ctx)
        }
        Expression::CallBackCall { callback, arguments } => {
            let callback_access = access_member(callback, ctx);
            let args = arguments.iter().map(|a| compile_expression(a, ctx)).collect::<Vec<_>>();
            format!("{}?.Invoke({})", callback_access, args.join(", "))
        }
        Expression::Cast { from, to } => {
            let expr = compile_expression(from, ctx);
            match (from.ty(ctx), to) {
                (Type::Float32, Type::Int32) => format!("(int){}", expr),
                (Type::Int32, Type::Float32) => format!("(float){}", expr),
                _ => expr,
            }
        }
        Expression::BinaryExpression { lhs, rhs, op } => {
            let lhs_code = compile_expression(lhs, ctx);
            let rhs_code = compile_expression(rhs, ctx);
            let mut buffer = [0; 3];
            let op_str = match op {
                '=' => "==",
                '!' => "!=",
                '≤' => "<=",
                '≥' => ">=",
                '&' => "&&",
                '|' => "||",
                _ => {
                    op.encode_utf8(&mut buffer)
                }
            };
            format!("({} {} {})", lhs_code, op_str, rhs_code)
        }
        Expression::UnaryOp { sub, op } => {
            format!("({}{})", op, compile_expression(sub, ctx))
        }
        Expression::Condition { condition, true_expr, false_expr } => {
            let cond = compile_expression(condition, ctx);
            let true_code = compile_expression(true_expr, ctx);
            let false_code = compile_expression(false_expr, ctx);
            format!("({} ? {} : {})", cond, true_code, false_code)
        }
        Expression::Array { values, element_ty, as_model: _ } => {
            let elements = values.iter().map(|v| compile_expression(v, ctx)).collect::<Vec<_>>();
            let element_type = element_ty.csharp_type().unwrap_or_else(|| "object".into());
            format!("new {}[] {{ {} }}", element_type, elements.join(", "))
        }
        Expression::Struct { ty, values } => {
            if ty.name.is_none() {
                // Anonymous struct - use tuple syntax
                let elements = ty.fields.keys().map(|k| {
                    values.get(k)
                        .map(|e| compile_expression(e, ctx))
                        .unwrap_or_else(|| "default".to_string())
                }).collect::<Vec<_>>();
                format!("({})", elements.join(", "))
            } else {
                // Named struct - use object initializer
                let struct_name = pascal_case(ty.name.as_ref().unwrap());
                let fields = values.iter().map(|(k, v)| {
                    format!("{} = {}", pascal_case(k), compile_expression(v, ctx))
                }).collect::<Vec<_>>();
                format!("new {} {{ {} }}", struct_name, fields.join(", "))
            }
        }
        Expression::EnumerationValue(value) => {
            let enum_name = if value.enumeration.node.is_some() {
                pascal_case(&value.enumeration.name)
            } else {
                pascal_case(&value.enumeration.name)
            };
            let value_name = pascal_case(&value.to_pascal_case());
            format!("{}.{}", enum_name, value_name)
        }
        Expression::EasingCurve(curve) => {
            match curve {
                EasingCurve::Linear => "Slint.Interop.EasingCurve.Linear".to_string(),
                EasingCurve::CubicBezier(a, b, c, d) => {
                    format!("Slint.Interop.EasingCurve.CubicBezier({}, {}, {}, {})", a, b, c, d)
                }
                EasingCurve::EaseInElastic => ".Interop.EasingCurve.EaseInElastic".to_string(),
                EasingCurve::EaseOutElastic => "Slint.Interop.EasingCurve.EaseOutElastic".to_string(),
                EasingCurve::EaseInOutElastic => "Slint.Interop.EasingCurve.EaseInOutElastic".to_string(),
                EasingCurve::EaseInBounce => "Slint.Interop.EasingCurve.EaseInBounce".to_string(),
                EasingCurve::EaseOutBounce => "Slint.Interop.EasingCurve.EaseOutBounce".to_string(),
                EasingCurve::EaseInOutBounce => "Slint.Interop.EasingCurve.EaseInOutBounce".to_string(),
            }
        }
        Expression::LinearGradient { angle, stops } => {
            let angle_code = compile_expression(angle, ctx);
            let stops_code = stops.iter().map(|(color, stop)| {
                let color_code = compile_expression(color, ctx);
                let stop_code = compile_expression(stop, ctx);
                format!("new Slint.Interop.GradientStop({}, {})", color_code, stop_code)
            }).collect::<Vec<_>>();
            format!("new Slint.Interop.LinearGradient({}, new[] {{ {} }})", angle_code, stops_code.join(", "))
        }
        Expression::RadialGradient { stops } => {
            let stops_code = stops.iter().map(|(color, stop)| {
                let color_code = compile_expression(color, ctx);
                let stop_code = compile_expression(stop, ctx);
                format!("new Slint.Interop.GradientStop({}, {})", color_code, stop_code)
            }).collect::<Vec<_>>();
            format!("new Slint.Interop.RadialGradient(new[] {{ {} }})", stops_code.join(", "))
        }
        Expression::ImageReference { resource_ref, nine_slice } => {
            let image_code = match resource_ref {
                crate::expression_tree::ImageReference::None => "Slint.Interop.Image.Empty".to_string(),
                crate::expression_tree::ImageReference::AbsolutePath(path) => {
                    format!(r#"Slint.Interop.Image.LoadFromPath("{}")"#, escape_string(path.as_str()))
                }
                crate::expression_tree::ImageReference::EmbeddedData { resource_id, extension: _ } => {
                    format!("Slint.Interop.Image.LoadFromEmbeddedData(EmbeddedResources.Resource{})", resource_id)
                }
                crate::expression_tree::ImageReference::EmbeddedTexture { resource_id } => {
                    format!("Slint.Interop.Image.LoadFromEmbeddedTexture(EmbeddedTextures.Texture{})", resource_id)
                }
            };
            
            match nine_slice {
                Some([a, b, c, d]) => {
                    format!("{}.WithNineSlice({}, {}, {}, {})", image_code, a, b, c, d)
                }
                None => image_code,
            }
        }
        _ => {
            // For expressions not yet implemented, return a placeholder
            format!("/* TODO: {} */default", std::any::type_name::<llr::Expression>())
        }
    }
}

fn compile_builtin_function_call(
    function: BuiltinFunction,
    arguments: &[llr::Expression],
    ctx: &EvaluationContext,
) -> String {
    let args = arguments.iter().map(|a| compile_expression(a, ctx)).collect::<Vec<_>>();
    
    match function {
        BuiltinFunction::Debug => {
            format!("System.Console.WriteLine({})", args.join(" + "))
        }
        BuiltinFunction::Mod => {
            format!("({} % {})", args[0], args[1])
        }
        BuiltinFunction::Round => {
            format!("Math.Round({})", args[0])
        }
        BuiltinFunction::Ceil => {
            format!("Math.Ceiling({})", args[0])
        }
        BuiltinFunction::Floor => {
            format!("Math.Floor({})", args[0])
        }
        BuiltinFunction::Sqrt => {
            format!("Math.Sqrt({})", args[0])
        }
        BuiltinFunction::Abs => {
            format!("Math.Abs({})", args[0])
        }
        BuiltinFunction::Sin => {
            format!("Math.Sin({} * Math.PI / 180.0)", args[0])
        }
        BuiltinFunction::Cos => {
            format!("Math.Cos({} * Math.PI / 180.0)", args[0])
        }
        BuiltinFunction::Tan => {
            format!("Math.Tan({} * Math.PI / 180.0)", args[0])
        }
        BuiltinFunction::ASin => {
            format!("Math.Asin({}) * 180.0 / Math.PI", args[0])
        }
        BuiltinFunction::ACos => {
            format!("Math.Acos({}) * 180.0 / Math.PI", args[0])
        }
        BuiltinFunction::ATan => {
            format!("Math.Atan({}) * 180.0 / Math.PI", args[0])
        }
        BuiltinFunction::ATan2 => {
            format!("Math.Atan2({}, {}) * 180.0 / Math.PI", args[0], args[1])
        }
        BuiltinFunction::Log => {
            format!("Math.Log({}, {})", args[0], args[1])
        }
        BuiltinFunction::Ln => {
            format!("Math.Log({})", args[0])
        }
        BuiltinFunction::Pow => {
            format!("Math.Pow({}, {})", args[0], args[1])
        }
        BuiltinFunction::Exp => {
            format!("Math.Exp({})", args[0])
        }
        BuiltinFunction::StringToFloat => {
            format!("float.Parse({})", args[0])
        }
        BuiltinFunction::StringIsFloat => {
            format!("float.TryParse({}, out _)", args[0])
        }
        BuiltinFunction::StringIsEmpty => {
            format!("string.IsNullOrEmpty({})", args[0])
        }
        BuiltinFunction::StringToLowercase => {
            format!("{}.ToLowerInvariant()", args[0])
        }
        BuiltinFunction::StringToUppercase => {
            format!("{}.ToUpperInvariant()", args[0])
        }
        BuiltinFunction::ArrayLength => {
            ctx.generator_state.conditional_usings.system_linq.set(true);
            format!("{}.Count", args[0])
        }
        BuiltinFunction::Rgb => {
            format!("Slint.Interop.Color.FromArgb({}, {}, {}, {})", args[3], args[0], args[1], args[2])
        }
        BuiltinFunction::Hsv => {
            format!("Slint.Interop.Color.FromHsva({}, {}, {}, {})", args[0], args[1], args[2], args[3])
        }
        BuiltinFunction::ColorBrighter => {
            format!("{}.Brighter({})", args[0], args[1])
        }
        BuiltinFunction::ColorDarker => {
            format!("{}.Darker({})", args[0], args[1])
        }
        BuiltinFunction::ImageSize => {
            format!("{}.Size", args[0])
        }
        BuiltinFunction::SetFocusItem => {
            "Window.SetFocusItem(/* item */)".to_string()
        }
        BuiltinFunction::ClearFocusItem => {
            "Window.ClearFocusItem()".to_string()
        }
        BuiltinFunction::ShowPopupWindow => {
            "Window.ShowPopup(/* popup */)".to_string()
        }
        BuiltinFunction::ClosePopupWindow => {
            "Window.ClosePopup(/* popup */)".to_string()
        }
        BuiltinFunction::GetWindowScaleFactor => {
            "Window.ScaleFactor".to_string()
        }
        BuiltinFunction::GetWindowDefaultFontSize => {
            "Window.DefaultFontSize".to_string()
        }
        BuiltinFunction::AnimationTick => {
            "Slint.Interop.AnimationTick()".to_string()
        }
        _ => {
            // For functions not yet implemented, return a placeholder
            format!("/* TODO: {:?} */default", function)
        }
    }
}

fn access_member(reference: &llr::PropertyReference, ctx: &EvaluationContext) -> String {
    match reference {
        llr::PropertyReference::Local { sub_component_path: _, property_index } => {
            if let Some(sub_component) = ctx.current_sub_component {
                let component = &ctx.compilation_unit.sub_components[sub_component];
                let property_name = pascal_case(&component.properties[*property_index].name);
                property_name.to_string()
            } else if let Some(current_global) = ctx.current_global() {
                let property_name = pascal_case(&current_global.properties[*property_index].name);
                property_name.to_string()
            } else {
                "/* unknown property */".to_string()
            }
        }
        llr::PropertyReference::Global { global_index, property_index } => {
            let global = &ctx.compilation_unit.globals[*global_index];
            let global_name = pascal_case(&global.name);
            let property_name = pascal_case(&global.properties[*property_index].name);
            format!("{}.{}", global_name, property_name)
        }
        llr::PropertyReference::InNativeItem { item_index: _, prop_name, .. } => {
            format!("/* native item property: {} */", prop_name)
        }
        llr::PropertyReference::InParent { .. } => {
            "/* parent property */".to_string()
        }
        llr::PropertyReference::Function { .. } => {
            "/* function reference */".to_string()
        }
        llr::PropertyReference::GlobalFunction { .. } => {
            "/* global function reference */".to_string()
        }
    }
}

fn property_set_value_code(
    property: &llr::PropertyReference,
    value_expr: &str,
    ctx: &EvaluationContext,
) -> String {
    let prop = access_member(property, ctx);
    format!("{} = {}", prop, value_expr)
}

fn handle_property_init(
    prop: &llr::PropertyReference,
    binding_expression: &llr::BindingExpression,
    init: &mut Vec<String>,
    ctx: &EvaluationContext,
) {
    let prop_access = access_member(prop, ctx);
    let prop_type = ctx.property_ty(prop);

    if let Type::Callback(_) = &prop_type {
        let binding_code = compile_expression(&binding_expression.expression.borrow(), ctx);
        init.push(format!("{} += {};", prop_access, binding_code));
    } else {
        let init_expr = compile_expression(&binding_expression.expression.borrow(), ctx);
        if binding_expression.is_constant {
            init.push(format!("{} = {};", prop_access, init_expr));
        } else {
            // For non-constant bindings, we'd need to set up property binding
            init.push(format!("SetBinding(\"{}\", () => {});", prop_access, init_expr));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ident() {
        assert_eq!(ident("hello"), "hello");
        assert_eq!(ident("hello-world"), "hello_world");
        assert_eq!(ident("class"), "@class");
        assert_eq!(ident("namespace"), "@namespace");
    }

    #[test]
    fn test_pascal_case() {
        assert_eq!(pascal_case("hello"), "Hello");
        assert_eq!(pascal_case("hello-world"), "HelloWorld");
        assert_eq!(pascal_case("hello_world"), "HelloWorld");
        assert_eq!(pascal_case("class"), "@Class");
    }

    #[test]
    fn test_csharp_keywords() {
        assert!(is_csharp_keyword("class"));
        assert!(is_csharp_keyword("namespace"));
        assert!(is_csharp_keyword("public"));
        assert!(!is_csharp_keyword("hello"));
    }
}