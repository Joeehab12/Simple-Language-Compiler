typedef enum { typeCon, typeId, typeOpr } nodeEnum;

/* constants */
typedef struct {
    int value;                  /* value of constant */
    int registerNumber;         /* register number */
} conNodeType;

/* identifiers */
typedef struct {
    char* id_name;                      /* subscript to sym array */
    int registerNumber;
} idNodeType;

/* operators */
typedef struct {
    int oper;                   /* operator */
    int nops;                   /* number of operands */
    struct nodeTypeTag *op[1];  /* operands, extended at runtime */
    int registerNumber;    /* register number */
} oprNodeType;

typedef struct nodeTypeTag {
    nodeEnum type;              /* type of node */

    union {
        conNodeType con;        /* constants */
        idNodeType id;          /* identifiers */
        oprNodeType opr;       /* operators */

    };
    int registerNumber;
} nodeType;

//extern int sym[26];