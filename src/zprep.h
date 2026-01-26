
#ifndef ZPREP_H
#define ZPREP_H

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __COSMOPOLITAN__
#include <cosmo.h>
#define z_is_windows() IsWindows()
#else
#ifdef _WIN32
#define z_is_windows() 1
#else
#define z_is_windows() 0
#endif
#endif

#ifdef _WIN32
#include <sys/types.h>
#ifndef PATH_MAX
#define PATH_MAX 260
#endif
#define realpath(N, R) _fullpath((R), (N), PATH_MAX)
#endif

// **ZEN VERSION**
#ifndef ZEN_VERSION
#define ZEN_VERSION "0.1.0"
#endif

// ** ANSI COLORS **
#define COLOR_RESET "\033[0m"
#define COLOR_RED "\033[1;31m"
#define COLOR_GREEN "\033[1;32m"
#define COLOR_YELLOW "\033[1;33m"
#define COLOR_BLUE "\033[1;34m"
#define COLOR_CYAN "\033[1;36m"
#define COLOR_BOLD "\033[1m"

// ** MEMORY OVERRIDES (Arena) **
#define free(ptr) ((void)0)
#define malloc(sz) xmalloc(sz)
#define realloc(p, s) xrealloc(p, s)
#define calloc(n, s) xcalloc(n, s)

// ** GLOBAL STATE **
extern char *g_current_filename;

/**
 * @brief Token types for the Lexer.
 */
typedef enum
{
    TOK_EOF = 0,    ///< End of File.
    TOK_IDENT,      ///< Identifier (variable, function name).
    TOK_INT,        ///< Integer literal.
    TOK_FLOAT,      ///< Float literal.
    TOK_STRING,     ///< String literal.
    TOK_FSTRING,    ///< Formatted string literal (f"val is {x}").
    TOK_CHAR,       ///< Character literal.
    TOK_LPAREN,     ///< (
    TOK_RPAREN,     ///< )
    TOK_LBRACE,     ///< {
    TOK_RBRACE,     ///< }
    TOK_LBRACKET,   ///< [
    TOK_RBRACKET,   ///< ]
    TOK_LANGLE,     ///< <
    TOK_RANGLE,     ///< >
    TOK_COMMA,      ///< ,
    TOK_COLON,      ///< :
    TOK_SEMICOLON,  ///< ;
    TOK_OP,         ///< General operator (e.g. +, *, /).
    TOK_AT,         ///< @
    TOK_DOTDOT,     ///< ..
    TOK_DOTDOT_EQ,  ///< ..= (inclusive range).
    TOK_DOTDOT_LT,  ///< ..< (exclusive range, explicit).
    TOK_ARROW,      ///< -> or =>
    TOK_PIPE,       ///< |> (pipe operator).
    TOK_TEST,       ///< 'test' keyword.
    TOK_ASSERT,     ///< 'assert' keyword.
    TOK_SIZEOF,     ///< 'sizeof' keyword.
    TOK_DEF,        ///< 'def' keyword.
    TOK_DEFER,      ///< 'defer' keyword.
    TOK_AUTOFREE,   ///< 'autofree' keyword.
    TOK_QUESTION,   ///< ?
    TOK_USE,        ///< 'use' keyword.
    TOK_QQ,         ///< ?? (null coalescing).
    TOK_QQ_EQ,      ///< ??=
    TOK_Q_DOT,      ///< ?. (optional chaining).
    TOK_DCOLON,     ///< ::
    TOK_TRAIT,      ///< 'trait' keyword.
    TOK_IMPL,       ///< 'impl' keyword.
    TOK_AND,        ///< 'and' keyword.
    TOK_OR,         ///< 'or' keyword.
    TOK_FOR,        ///< 'for' keyword.
    TOK_COMPTIME,   ///< 'comptime' keyword.
    TOK_ELLIPSIS,   ///< ...
    TOK_UNION,      ///< 'union' keyword.
    TOK_ASM,        ///< 'asm' keyword.
    TOK_VOLATILE,   ///< 'volatile' keyword.
    TOK_ASYNC,      ///< 'async' keyword.
    TOK_AWAIT,      ///< 'await' keyword.
    TOK_PREPROC,    ///< Preprocessor directive (#...).
    TOK_ALIAS,      ///< 'alias' keyword.
    TOK_COMMENT,    ///< Comment (usually skipped).
    TOK_UNKNOWN     ///< Unknown token.
} TokenType;

/**
 * @brief Represents a source token.
 */
typedef struct
{
    TokenType type;     ///< Type of the token.
    const char *start;  ///< Pointer to start of token in source buffer.
    int len;            ///< Length of the token text.
    int line;           ///< Line number (1-based).
    int col;            ///< Column number (1-based).
} Token;

/**
 * @brief Lexer state.
 */
typedef struct
{
    const char *src;    ///< Source code buffer.
    int pos;            ///< Current position index.
    int line;           ///< Current line number.
    int col;            ///< Current column number.
} Lexer;

void lexer_init(Lexer *l, const char *src);
Token lexer_next(Lexer *l);
Token lexer_peek(Lexer *l);
Token lexer_peek2(Lexer *l);

void register_trait(const char *name);
int is_trait(const char *name);

// Arena and memory.
void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t new_size);
void *xcalloc(size_t n, size_t size);
char *xstrdup(const char *s);

// Error reporting.
void zpanic(const char *fmt, ...);
void zpanic_at(Token t, const char *fmt, ...);

char *load_file(const char *filename);

// ** Buffer Size Constants **
#define MAX_FLAGS_SIZE 1024
#define MAX_PATH_SIZE 1024
#define MAX_PATTERN_SIZE 1024

// ** Build Directives **
extern char g_link_flags[MAX_FLAGS_SIZE];
extern char g_cflags[MAX_FLAGS_SIZE];
extern int g_warning_count;

struct ParserContext;

void scan_build_directives(struct ParserContext *ctx, const char *src);
int levenshtein(const char *s1, const char *s2);
void zpanic_with_suggestion(Token t, const char *msg, const char *suggestion);

// Specific error types.
void error_undefined_function(Token t, const char *func_name, const char *suggestion);
void error_wrong_arg_count(Token t, const char *func_name, int expected, int got);
void error_undefined_field(Token t, const char *struct_name, const char *field_name,
                           const char *suggestion);
void error_type_expected(Token t, const char *expected, const char *got);
void error_cannot_index(Token t, const char *type_name);

// Warning system.
void zwarn(const char *fmt, ...);
void zwarn_at(Token t, const char *fmt, ...);

// Specific warnings.
void warn_unused_variable(Token t, const char *var_name);
void warn_unused_parameter(Token t, const char *param_name, const char *func_name);
void warn_shadowing(Token t, const char *var_name);
void warn_unreachable_code(Token t);
void warn_implicit_conversion(Token t, const char *from_type, const char *to_type);
void warn_narrowing_conversion(Token t, const char *from_type, const char *to_type);
void warn_missing_return(Token t, const char *func_name);
void warn_comparison_always_true(Token t, const char *reason);
void warn_comparison_always_false(Token t, const char *reason);
void warn_division_by_zero(Token t);
void warn_integer_overflow(Token t, const char *type_name, long long value);
void warn_array_bounds(Token t, int index, int size);
void warn_format_string(Token t, int arg_num, const char *expected, const char *got);
void warn_null_pointer(Token t, const char *expr);

/**
 * @brief Compiler configuration and flags.
 */
typedef struct
{
    char *input_file;       ///< Input source file path.
    char *output_file;      ///< Output binary file path.

    // Modes.
    int mode_run;           ///< 1 if 'run' command (compile & execute).
    int mode_check;         ///< 1 if 'check' command (syntax/type check only).
    int emit_c;             ///< 1 if --emit-c (keep generated C file).
    int verbose;            ///< 1 if --verbose.
    int quiet;              ///< 1 if --quiet.
    int no_zen;             ///< 1 if --no-zen (disable zen facts/easter eggs).
    int repl_mode;          ///< 1 if --repl (internal flag for REPL usage).
    int is_freestanding;    ///< 1 if --freestanding (no stdlib).
    int mode_transpile;     ///< 1 if 'transpile' command (to C).
    int use_cpp;            ///< 1 if --cpp (emit C++ compatible code).
    int use_cuda;           ///< 1 if --cuda (emit CUDA-compatible code).

    // GCC Flags accumulator.
    char gcc_flags[4096];   ///< Flags passed to the backend compiler.

    // C Compiler selection (default: gcc)
    char cc[64];            ///< Backend compiler command (e.g. "gcc", "clang").
} CompilerConfig;

extern CompilerConfig g_config;
extern char g_link_flags[];
extern char g_cflags[];

struct ParserContext;
void scan_build_directives(struct ParserContext *ctx, const char *src);

#endif
