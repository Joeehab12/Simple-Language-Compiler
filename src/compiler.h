typedef enum { typeCon, typeId, typeOpr } nodeEnum;
typedef enum { integer_num, float_num } typeEnum;
/* constants */
typedef struct {
    int value;                  /* value of constant */
    typeEnum type;
    float fvalue;
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
    int caseNum;
    int registerNumber;
    int declarationFlag;
    int conditionFlag;
} nodeType;

//extern int sym[26];