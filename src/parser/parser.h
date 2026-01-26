
#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "zprep.h"

// Operator precedence for expression parsing
typedef enum
{
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_TERNARY,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY
} Precedence;

// Main entry points
// Forward declarations
struct ParserContext;
typedef struct ParserContext ParserContext;

ASTNode *parse_program(ParserContext *ctx, Lexer *l);

extern ParserContext *g_parser_ctx;

// Symbol table
/**
 * @brief Represents a symbol in the symbol table.
 * 
 * Used for variables, functions, and other named entities.
 */
typedef struct ZenSymbol
{
    char *name;             ///< Symbol name.
    char *type_name;        ///< String representation of the type.
    Type *type_info;        ///< Formal type definition.
    int is_used;            ///< 1 if the symbol has been referenced.
    int is_autofree;        ///< 1 if it requires automatic memory management (RAII).
    Token decl_token;       ///< Token where the symbol was declared.
    int is_const_value;     ///< 1 if it is a compile-time constant.
    int is_def;             ///< 1 if it is a definition (vs declaration).
    int const_int_val;      ///< Integer value if it is a constant.
    int is_moved;           ///< 1 if the value has been moved (ownership transfer).
    struct ZenSymbol *next; ///< Next symbol in the bucket/list (chaining).
} ZenSymbol;

/**
 * @brief Represents a lexical scope (block).
 * 
 * Scopes form a hierarchy (parent pointer) and contain a list of symbols defined in that scope.
 */
typedef struct Scope
{
    ZenSymbol *symbols;     ///< Linked list of symbols in this scope.
    struct Scope *parent;   ///< Pointer to the parent scope (NULL for global).
} Scope;

/**
 * @brief Registry entry for a function signature.
 * 
 * Stores metadata about declared functions for type checking and call validation.
 */
typedef struct FuncSig
{
    char *name;             ///< Function name.
    Token decl_token;       ///< declaration token.
    int total_args;         ///< Total argument count.
    char **defaults;        ///< Default values for arguments (or NULL).
    Type **arg_types;       ///< Argument types.
    Type *ret_type;         ///< Return type.
    int is_varargs;         ///< 1 if variadic.
    int is_async;           ///< 1 if async.
    int must_use;           ///< 1 if return value must be used.
    struct FuncSig *next;   ///< Next function in registry.
} FuncSig;

/**
 * @brief Tracks a lambda (anonymous function) within the parser.
 */
typedef struct LambdaRef
{
    ASTNode *node;          ///< The AST node for the lambda.
    struct LambdaRef *next;
} LambdaRef;

/**
 * @brief Template for a generic struct.
 */
typedef struct GenericTemplate
{
    char *name;             ///< Template name.
    ASTNode *struct_node;   ///< The struct AST node (containing generic params).
    struct GenericTemplate *next;
} GenericTemplate;

/**
 * @brief Template for a generic function.
 */
typedef struct GenericFuncTemplate
{
    char *name;             ///< Template name.
    char *generic_param;    ///< Generic parameters string (legacy).
    ASTNode *func_node;     ///< The function AST node.
    struct GenericFuncTemplate *next;
} GenericFuncTemplate;

/**
 * @brief Template for a generic implementation block.
 */
typedef struct GenericImplTemplate
{
    char *struct_name;      ///< Target struct name.
    char *generic_param;    ///< Generic parameters.
    ASTNode *impl_node;     ///< The impl block AST node.
    struct GenericImplTemplate *next;
} GenericImplTemplate;

/**
 * @brief Represents an imported source file (to prevent cycles/duplication).
 */
typedef struct ImportedFile
{
    char *path;             ///< Absolute file path.
    struct ImportedFile *next;
} ImportedFile;

/**
 * @brief Tracks a concrete instantiation of a generic template.
 */
typedef struct Instantiation
{
    char *name;             ///< Mangled name of the instantiation (e.g. "Vec_int").
    char *template_name;    ///< Original template name (e.g. "Vec").
    char *concrete_arg;     ///< Concrete type argument string.
    char *unmangled_arg;    ///< Unmangled argument for substitution code.
    ASTNode *struct_node;   ///< The AST node of the instantiated struct.
    struct Instantiation *next;
} Instantiation;

/**
 * @brief Reference to a parsed struct (list node).
 */
typedef struct StructRef
{
    ASTNode *node;
    struct StructRef *next;
} StructRef;

/**
 * @brief Definition of a struct (lookup cache).
 */
typedef struct StructDef
{
    char *name;
    ASTNode *node;
    struct StructDef *next;
} StructDef;

/**
 * @brief Track used slice types for generation.
 */
typedef struct SliceType
{
    char *name;
    struct SliceType *next;
} SliceType;

/**
 * @brief Track used tuple signatures for generation.
 */
typedef struct TupleType
{
    char *sig;
    struct TupleType *next;
} TupleType;

/**
 * @brief Registry of enum variants.
 */
typedef struct EnumVariantReg
{
    char *enum_name;        ///< Name of the enum.
    char *variant_name;     ///< Name of the variant.
    int tag_id;             ///< Integration tag value.
    struct EnumVariantReg *next;
} EnumVariantReg;

/**
 * @brief Functions marked as deprecated.
 */
typedef struct DeprecatedFunc
{
    char *name;
    char *reason;           ///< Optional reason for deprecation.
    struct DeprecatedFunc *next;
} DeprecatedFunc;

/**
 * @brief Represents a module (namespace/file).
 */
typedef struct Module
{
    char *alias;            ///< Import alias (or default name).
    char *path;             ///< File path.
    char *base_name;        ///< Base name of the module.
    int is_c_header;        ///< 1 if this is a C header import.
    struct Module *next;
} Module;

/**
 * @brief Symbol imported via selective import (import { X }).
 */
typedef struct SelectiveImport
{
    char *symbol;           ///< Symbol name.
    char *alias;            ///< Local alias.
    char *source_module;    ///< Origin module.
    struct SelectiveImport *next;
} SelectiveImport;

/**
 * @brief Registry for trait implementations.
 */
typedef struct ImplReg
{
    char *trait;            ///< Trait name.
    char *strct;            ///< Implementing struct name.
    struct ImplReg *next;
} ImplReg;

/**
 * @brief Loaded compiler plugin.
 */
typedef struct ImportedPlugin
{
    char *name;             ///< Plugin name (e.g., "brainfuck").
    char *alias;            ///< Optional usage alias.
    struct ImportedPlugin *next;
} ImportedPlugin;

/**
 * @brief Type alias definition.
 */
typedef struct TypeAlias
{
    char *alias;            ///< New type name.
    char *original_type;    ///< Original type.
    struct TypeAlias *next;
} TypeAlias;

/**
 * @brief Global compilation state and symbol table.
 * 
 * ParserContext maintains the state of the compiler during parsing and analysis.
 * It holds symbol tables, type definitions, function registries, and configuration.
 */
struct ParserContext
{
    Scope *current_scope;       ///< Current lexical scope for variable lookup.
    FuncSig *func_registry;     ///< Registry of declared function signatures.

    // Lambdas
    LambdaRef *global_lambdas;  ///< List of all lambdas generated during parsing.
    int lambda_counter;         ///< Counter for generating unique lambda IDs.

// Generics
#define MAX_KNOWN_GENERICS 1024
    char *known_generics[MAX_KNOWN_GENERICS]; ///< Stack of currently active generic type parameters.
    int known_generics_count;                 ///< Count of active generic parameters.
    GenericTemplate *templates;               ///< Struct generic templates.
    GenericFuncTemplate *func_templates;      ///< Function generic templates.
    GenericImplTemplate *impl_templates;      ///< Implementation block templates.

    // Instantiations
    Instantiation *instantiations;      ///< Cache of instantiated generic types.
    ASTNode *instantiated_structs;      ///< List of AST nodes for instantiated structs.
    ASTNode *instantiated_funcs;        ///< List of AST nodes for instantiated functions.

    // Structs/Enums
    StructRef *parsed_structs_list;     ///< List of all parsed struct nodes.
    StructRef *parsed_enums_list;       ///< List of all parsed enum nodes.
    StructRef *parsed_funcs_list;       ///< List of all parsed function nodes.
    StructRef *parsed_impls_list;       ///< List of all parsed impl blocks.
    StructRef *parsed_globals_list;     ///< List of all parsed global variables.
    StructDef *struct_defs;             ///< Registry of struct definitions (map name -> node).
    EnumVariantReg *enum_variants;      ///< Registry of enum variants for global lookup.
    ImplReg *registered_impls;          ///< Cache of type/trait implementations.

    // Types
    SliceType *used_slices;             ///< Cache of generated slice types.
    TupleType *used_tuples;             ///< Cache of generated tuple types.
    TypeAlias *type_aliases;            ///< Defined type aliases.

    // Modules/Imports
    Module *modules;                    ///< List of registered modules.
    SelectiveImport *selective_imports; ///< Symbols imported via `import { ... }`.
    char *current_module_prefix;        ///< Prefix for current module (namespacing).
    ImportedFile *imported_files;       ///< List of files already included/imported.
    ImportedPlugin *imported_plugins;   ///< List of active plugins.

    // Config/State
    char *current_impl_struct;          ///< Name of struct currently being implemented (in impl block).
    ASTNode *current_impl_methods;      ///< Head of method list for current impl block.

    // Internal tracking
    DeprecatedFunc *deprecated_funcs;   ///< Registry of deprecated functions.

    // LSP / Fault Tolerance
    int is_fault_tolerant;              ///< 1 if parser should recover from errors (LSP mode).
    void *error_callback_data;          ///< User data for error callback.
    void (*on_error)(void *data, Token t, const char *msg); ///< Callback for reporting errors.

    // LSP: Flat symbol list (persists after parsing for LSP queries)
    ZenSymbol *all_symbols;             ///< comprehensive list of all symbols seen.

    // External C interop: suppress undefined warnings for external symbols
    int has_external_includes;          ///< Set when `#include <...>` is used.
    char **extern_symbols;              ///< Explicitly declared extern symbols.
    int extern_symbol_count;            ///< Count of external symbols.

    // Codegen state:
    FILE *hoist_out;    ///< File stream for hoisting code (e.g. from plugins).
    int skip_preamble;  ///< If 1, codegen won't emit standard preamble (includes etc).
    int is_repl;        ///< 1 if running in REPL mode.
    int has_async;      ///< 1 if async/await features are used in the program.
    int in_defer_block; ///< 1 if currently parsing inside a defer block.

    // Type Validation
    struct TypeUsage *pending_type_validations; ///< List of types to validate after parsing.
    int is_speculative;  ///< Flag to suppress side effects during speculative parsing.
    int silent_warnings; ///< Suppress warnings (e.g., during codegen interpolation).
};

typedef struct TypeUsage
{
    char *name;
    Token location;
    struct TypeUsage *next;
} TypeUsage;

// Type validation prototypes
void register_type_usage(ParserContext *ctx, const char *name, Token t);
int validate_types(ParserContext *ctx);

// Token helpers
char *token_strdup(Token t);
int is_token(Token t, const char *s);
Token expect(Lexer *l, TokenType type, const char *msg);
void skip_comments(Lexer *l);
char *consume_until_semicolon(Lexer *l);
char *consume_and_rewrite(ParserContext *ctx, Lexer *l);

// C reserved word warnings
int is_c_reserved_word(const char *name);
void warn_c_reserved_word(Token t, const char *name);

// ZenSymbol table
void enter_scope(ParserContext *ctx);
void exit_scope(ParserContext *ctx);
void add_symbol(ParserContext *ctx, const char *n, const char *t, Type *type_info);
void add_symbol_with_token(ParserContext *ctx, const char *n, const char *t, Type *type_info,
                           Token tok);
Type *find_symbol_type_info(ParserContext *ctx, const char *n);
char *find_symbol_type(ParserContext *ctx, const char *n);
ZenSymbol *find_symbol_entry(ParserContext *ctx, const char *n);
ZenSymbol *find_symbol_in_all(ParserContext *ctx,
                              const char *n);
char *find_similar_symbol(ParserContext *ctx, const char *name);

// Function registry
void register_func(ParserContext *ctx, const char *name, int count, char **defaults,
                   Type **arg_types, Type *ret_type, int is_varargs, int is_async,
                   Token decl_token);
void register_func_template(ParserContext *ctx, const char *name, const char *param, ASTNode *node);
GenericFuncTemplate *find_func_template(ParserContext *ctx, const char *name);

// Generic/template helpers
void register_generic(ParserContext *ctx, char *name);
int is_known_generic(ParserContext *ctx, char *name);
void register_impl_template(ParserContext *ctx, const char *sname, const char *param,
                            ASTNode *node);
void add_to_struct_list(ParserContext *ctx, ASTNode *node);
void add_to_enum_list(ParserContext *ctx, ASTNode *node);
void add_to_func_list(ParserContext *ctx, ASTNode *node);
void add_to_impl_list(ParserContext *ctx, ASTNode *node);
void add_to_global_list(ParserContext *ctx, ASTNode *node);
void register_builtins(ParserContext *ctx);
void add_instantiated_func(ParserContext *ctx, ASTNode *fn);
void instantiate_generic(ParserContext *ctx, const char *name, const char *concrete_type,
                         const char *unmangled_type, Token t);
void instantiate_generic_multi(ParserContext *ctx, const char *name, char **args, int arg_count,
                               Token t);
char *sanitize_mangled_name(const char *name);
void register_type_alias(ParserContext *ctx, const char *alias, const char *original);
const char *find_type_alias(ParserContext *ctx, const char *alias);
void register_impl(ParserContext *ctx, const char *trait, const char *strct);
int check_impl(ParserContext *ctx, const char *trait, const char *strct);
void register_template(ParserContext *ctx, const char *name, ASTNode *node);
void register_deprecated_func(ParserContext *ctx, const char *name, const char *reason);
DeprecatedFunc *find_deprecated_func(ParserContext *ctx, const char *name);
ASTNode *parse_arrow_lambda_single(ParserContext *ctx, Lexer *l, char *param_name);
ASTNode *parse_arrow_lambda_multi(ParserContext *ctx, Lexer *l, char **param_names, int num_params);

// Utils
char *parse_and_convert_args(ParserContext *ctx, Lexer *l, char ***defaults_out, int *count_out,
                             Type ***types_out, char ***names_out, int *is_varargs_out);
int is_file_imported(ParserContext *ctx, const char *path);
void mark_file_imported(ParserContext *ctx, const char *path);
void register_plugin(ParserContext *ctx, const char *name, const char *alias);
const char *resolve_plugin(ParserContext *ctx, const char *name_or_alias);
void print_type_defs(ParserContext *ctx, FILE *out, ASTNode *nodes);

// String manipulation
char *replace_in_string(const char *src, const char *old_w, const char *new_w);
char *replace_type_str(const char *src, const char *param, const char *concrete,
                       const char *old_struct, const char *new_struct);
Type *replace_type_formal(Type *t, const char *p, const char *c, const char *os, const char *ns);
ASTNode *copy_ast_replacing(ASTNode *n, const char *p, const char *c, const char *os,
                            const char *ns);
char *extract_module_name(const char *path);

// Enum helpers
void register_enum_variant(ParserContext *ctx, const char *ename, const char *vname, int tag);
EnumVariantReg *find_enum_variant(ParserContext *ctx, const char *vname);

// Lambda helpers
void register_lambda(ParserContext *ctx, ASTNode *node);
void analyze_lambda_captures(ParserContext *ctx, ASTNode *lambda);

// Type registration
void register_slice(ParserContext *ctx, const char *type);
void register_tuple(ParserContext *ctx, const char *sig);

// Struct lookup
ASTNode *find_struct_def(ParserContext *ctx, const char *name);
void register_struct_def(ParserContext *ctx, const char *name, ASTNode *node);

// Module system
Module *find_module(ParserContext *ctx, const char *alias);
void register_module(ParserContext *ctx, const char *alias, const char *path);
void register_selective_import(ParserContext *ctx, const char *symbol, const char *alias,
                               const char *source_module);
SelectiveImport *find_selective_import(ParserContext *ctx, const char *name);

// Type Aliases
void register_type_alias(ParserContext *ctx, const char *alias, const char *original);
const char *find_type_alias(ParserContext *ctx, const char *alias);

// External symbol tracking (C interop)
void register_extern_symbol(ParserContext *ctx, const char *name);
int is_extern_symbol(ParserContext *ctx, const char *name);
int should_suppress_undef_warning(ParserContext *ctx, const char *name);

// Initialization
void init_builtins();

// Expression rewriting
char *rewrite_expr_methods(ParserContext *ctx, char *raw);
char *process_fstring(ParserContext *ctx, const char *content, char ***used_syms, int *count);
char *instantiate_function_template(ParserContext *ctx, const char *name, const char *concrete_type,
                                    const char *unmangled_type);
FuncSig *find_func(ParserContext *ctx, const char *name);

Type *parse_type_formal(ParserContext *ctx, Lexer *l);
char *parse_type(ParserContext *ctx, Lexer *l);
Type *parse_type_base(ParserContext *ctx, Lexer *l);

ASTNode *parse_expression(ParserContext *ctx, Lexer *l);
ASTNode *parse_expr_prec(ParserContext *ctx, Lexer *l, Precedence min_prec);
ASTNode *parse_primary(ParserContext *ctx, Lexer *l);
ASTNode *parse_lambda(ParserContext *ctx, Lexer *l);
// parse_arrow_lambda_single/multi already declared above
char *parse_condition_raw(ParserContext *ctx, Lexer *l);
char *parse_array_literal(ParserContext *ctx, Lexer *l, const char *st);
char *parse_tuple_literal(ParserContext *ctx, Lexer *l, const char *tn);
ASTNode *parse_embed(ParserContext *ctx, Lexer *l);

ASTNode *parse_macro_call(ParserContext *ctx, Lexer *l, char *name);
ASTNode *parse_statement(ParserContext *ctx, Lexer *l);
ASTNode *parse_block(ParserContext *ctx, Lexer *l);
ASTNode *parse_if(ParserContext *ctx, Lexer *l);
ASTNode *parse_while(ParserContext *ctx, Lexer *l);
ASTNode *parse_for(ParserContext *ctx, Lexer *l);
ASTNode *parse_loop(ParserContext *ctx, Lexer *l);
ASTNode *parse_repeat(ParserContext *ctx, Lexer *l);
ASTNode *parse_unless(ParserContext *ctx, Lexer *l);
ASTNode *parse_guard(ParserContext *ctx, Lexer *l);
ASTNode *parse_match(ParserContext *ctx, Lexer *l);
ASTNode *parse_return(ParserContext *ctx, Lexer *l);

char *process_printf_sugar(ParserContext *ctx, const char *content, int newline, const char *target,
                           char ***used_syms, int *count, int check_symbols);
ASTNode *parse_assert(ParserContext *ctx, Lexer *l);
ASTNode *parse_defer(ParserContext *ctx, Lexer *l);
ASTNode *parse_asm(ParserContext *ctx, Lexer *l);
ASTNode *parse_plugin(ParserContext *ctx, Lexer *l);
ASTNode *parse_var_decl(ParserContext *ctx, Lexer *l);
ASTNode *parse_def(ParserContext *ctx, Lexer *l);
ASTNode *parse_type_alias(ParserContext *ctx, Lexer *l);

ASTNode *parse_function(ParserContext *ctx, Lexer *l, int is_async);
ASTNode *parse_struct(ParserContext *ctx, Lexer *l, int is_union);
ASTNode *parse_enum(ParserContext *ctx, Lexer *l);
ASTNode *parse_trait(ParserContext *ctx, Lexer *l);
ASTNode *parse_impl(ParserContext *ctx, Lexer *l);
ASTNode *parse_impl_trait(ParserContext *ctx, Lexer *l);
ASTNode *parse_test(ParserContext *ctx, Lexer *l);

// Move semantics helpers
int is_type_copy(ParserContext *ctx, Type *t);
void check_move_usage(ParserContext *ctx, ASTNode *node, Token t);
ASTNode *parse_include(ParserContext *ctx, Lexer *l);
ASTNode *parse_import(ParserContext *ctx, Lexer *l);
ASTNode *parse_comptime(ParserContext *ctx, Lexer *l);

char *patch_self_args(const char *args, const char *struct_name);

ASTNode *parse_program_nodes(ParserContext *ctx, Lexer *l);

#endif // PARSER_H
