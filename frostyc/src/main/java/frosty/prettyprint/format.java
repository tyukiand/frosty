// package frosty.prettyprint;
// 
// import java.lang.annotation.*;
// 
// 
// /**
//  * Specifies pretty-print format for <code>Product</code> classes.
//  * 
//  * The <code>format</code> should contain placeholders that look like
//  * <code>${n}</code> where <code>n</code> is an integer.
//  */
// // TODO: not clear, how to deal with delimited lists etc? Do I need an 
// // entire embedded DSL for the format specifiers? Omit it for now
// @Retention(RetentionPolicy.RUNTIME)
// @Target(ElementType.TYPE)
// public @interface format {
//     String value();
// }