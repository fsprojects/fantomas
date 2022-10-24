---
category: End-users
categoryindex: 1
index: 7
---
# JetBrains Rider
The resharper-fsharp uses Fantomas under the hood to format the source code. No need for any additional plugins.  
  
From Rider 2022.2 onwards, Rider can detect your `dotnet` Fantomas installation, either globally or locally.  
Install Fantomas locally with `dotnet tool install fantomas`.

<img class="mt-2" src="{{root}}/images/rider-fantomas.png" alt="drawing" width="70%"/>

At the time of writing, Rider [overwrites](https://youtrack.jetbrains.com/issue/RIDER-83997/Rider-doesnt-respect-Fantomas-default-settings-not-explicitly-set-in-editorconfig)
the settings of Fantomas which aren't explicitly set in your editorconfig file.  
As the settings chosen by Rider can lead to unwanted results, Rider users might want to also include the default settings in their editorconfig file.  
The current default settings are the following:

```
indent_size=4
max_line_length=120
end_of_line=crlf
insert_final_newline=true 
fsharp_space_before_parameter=true 
fsharp_space_before_lowercase_invocation=true 
fsharp_space_before_uppercase_invocation=false
fsharp_space_before_class_constructor=false
fsharp_space_before_member=false
fsharp_space_before_colon=false
fsharp_space_after_comma=true 
fsharp_space_before_semicolon=false
fsharp_space_after_semicolon=true 
fsharp_space_around_delimiter=true 
fsharp_max_if_then_short_width=0
fsharp_max_if_then_else_short_width=60
fsharp_max_infix_operator_expression=80
fsharp_max_record_width=40
fsharp_max_record_number_of_items=1
fsharp_record_multiline_formatter=character_width
fsharp_max_array_or_list_width=80
fsharp_max_array_or_list_number_of_items=1
fsharp_array_or_list_multiline_formatter=character_width
fsharp_max_value_binding_width=80
fsharp_max_function_binding_width=40
fsharp_max_dot_get_expression_width=80
fsharp_multiline_block_brackets_on_same_column=false
fsharp_newline_between_type_definition_and_members=true 
fsharp_align_function_signature_to_indentation=false
fsharp_alternative_long_member_definitions=false
fsharp_multi_line_lambda_closing_newline=false
fsharp_experimental_keep_indent_in_branch=false
fsharp_blank_lines_around_nested_multiline_expressions=true 
fsharp_bar_before_discriminated_union_declaration=false
fsharp_experimental_stroustrup_style=false
fsharp_keep_max_number_of_blank_lines=100
fsharp_strict_mode=false
```

<fantomas-nav previous="./GitHooks.html" next="./VisualStudio.html"></fantomas-nav>
