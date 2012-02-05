typedef struct Expression_t Expression_p;

typedef struct Add_t {
        Expression_p x;
        Expression_p y;
    } Add;

typedef struct Multiply_t {
        Expression_p x;
        Expression_p y;
    } Multiply;

typedef struct Lit_t {
    int value;
    } Lit;

typedef enum {
        EXPRESSION_TYPE_ADD,
        EXPRESSION_TYPE_MULTIPLY,
        EXPRESSION_TYPE_LIT
    } EExpressionType;


typedef struct Expression_t {
    EExpressionType type;
    
    union {
            Add add;
            Multiply multiply;
            Lit lit;
        };
    } Expression;
