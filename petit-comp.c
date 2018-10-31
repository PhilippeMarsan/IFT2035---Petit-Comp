/* fichier: "petit-comp.c" */

/* Un petit compilateur et machine virtuelle pour un sous-ensemble de C.  */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

/*---------------------------------------------------------------------------*/

/* Analyseur lexical. */

void memory_dealloc(); //Forward declaration

int error = 0; //Valide si le programme peut s'executer ou non

int l_num = 1;  //compte la ligne du code analyser

enum { DO_SYM, ELSE_SYM, IF_SYM, WHILE_SYM, PRINT_SYM, GOTO_SYM, BRK_SYM, CON_SYM, LBRA, RBRA, LPAR,
       RPAR, PLUS, MINUS, LESS, SEMI, EQUAL, INT, ID, EOI, STAR, PERC, SLASH,
       GREATER, EXCLM, COLON};

char *words[] = { "do", "else", "if", "while", "print","goto", "break", "continue",NULL };

int ch = ' ';
int sym;
int int_val;
char id_name[100];

void syntax_error(int cas)
{
  switch (cas)
  {
  case 1:  printf("Problem with end of file on line %d \n", l_num); break;
  case 2:  printf("Syntax Error: Missing a ';' on line %d \n", l_num); break;
  case 3:  printf("Syntax Error: Unknown instruction on line %d \n", l_num); break;
  case 4:  printf("Syntax Error: Unrecognizable character on line %d\n", l_num); break;
  case 5:  printf("Syntax Error: Invalid label name on line %d \n", l_num);break;
  case 6:  printf("Syntax Error: Expecting a paranthese ')' on line %d\n", l_num);break;
  case 7:  printf("Syntax Error: Expecting a paranthese '(' on line %d\n", l_num);break;
  case 8:  printf("Syntax Error: instruction or id name on line %d is too long\n", l_num);break;
  case 9:  printf("Syntax Error: Number out of the range[-128,127] can't be assigned at the start of the program see line %d\n", l_num);break;
  case 10: printf("Syntax Error: Memory Overflow. Can't analyse code past this line %d\n", l_num); memory_dealloc(); exit(1);
  default: printf("Syntax Error on line %d\n", l_num);
  }
  error = 1;
  return; }

void next_ch() { ch = getchar(); }

void next_sym()
{
  while (ch == ' ' || ch == '\n' || ch == '\t') {if (ch == '\n') l_num++; next_ch();}
  switch (ch)
    { case '{': sym = LBRA;    next_ch(); break;
      case '}': sym = RBRA;    next_ch(); break;
      case '(': sym = LPAR;    next_ch(); break;
      case ')': sym = RPAR;    next_ch(); break;
      case '+': sym = PLUS;    next_ch(); break;
      case '-': sym = MINUS;   next_ch(); break;
      case '<': sym = LESS;    next_ch(); break;
      case ';': sym = SEMI;    next_ch(); break;
      case '=': sym = EQUAL;   next_ch(); break;
      case '*': sym = STAR;    next_ch(); break;
      case '%': sym = PERC;    next_ch(); break;
      case '/': sym = SLASH;   next_ch(); break;
      case '>': sym = GREATER; next_ch(); break;
      case '!': sym = EXCLM;   next_ch(); break;
      case ':': sym = COLON;   next_ch(); break;
      case EOF: sym = EOI;     next_ch(); break;
      default:
        if (ch >= '0' && ch <= '9')
          {
            int_val = 0; /* la valeur doit etre comprise entre -128 et 127*/

            while (ch >= '0' && ch <= '9')
              {
                int_val = int_val*10 + (ch - '0');
                if (!(int_val > (-128) && int_val < 127)) syntax_error(9);
                next_ch();
              }

            sym = INT;
          }
        else if (ch >= 'a' && ch <= 'z')
          {
            int i = 0; /* un nom peu avoir au maximum 100 ch*/

            while ((ch >= 'a' && ch <= 'z') || ch == '_')
              {
                id_name[i++] = ch;
                if(i > 99) syntax_error(8);
                next_ch();
              }

            id_name[i] = '\0';
            sym = 0;

            while (words[sym]!=NULL && strcmp(words[sym], id_name)!=0)
              sym++;

            if (words[sym] == NULL)
              {
                if (id_name[1] == '\0') sym = ID; else syntax_error(3);
              }
          }
        else syntax_error(4);
    }
}

/*---------------------------------------------------------------------------*/

/* Analyseur syntaxique. */

enum { VAR, CST, ADD, SUB, LT, GT, NOEQ, EQ, LTEQ, GTEQ, ASSIGN,
       IF1, IF2, WHILE, DO, EMPTY, SEQ, EXPR, PROG, PRINT,
       MULT, MOD, DIV, LABEL, GOTON, BREAK, CONT};

struct node
  {
    int kind;
    struct node *o1;
    struct node *o2;
    struct node *o3;
    int val;
  };

typedef struct node node;

node *root;

node *new_node(int k)
{
  node *x = malloc(sizeof(node));
  if(x == NULL) syntax_error(10);
  x->kind = k;
  x->o1 = NULL;
  x->o2 = NULL;
  x->o3 = NULL;
  return x;
}

node *paren_expr(); /* forward declaration */
node *mult(); /* forward declaration */

node *term() /* <term> ::= <id> | <int> | <paren_expr> */
{
  node *x;

  if (sym == ID)           /* <term> ::= <id> */
    {
      x = new_node(VAR);
      x->val = id_name[0]-'a';
      next_sym();
    }
  else if (sym == INT)     /* <term> ::= <int> */
    {
      x = new_node(CST);
      x->val = int_val;
      next_sym();
    }
  else                     /* <term> ::= <paren_expr> */
    x = paren_expr();

  return x;
}

node *sum() /* <sum> ::= <term>|<sum>"+"<term>|<sum>"-"<term>|<sum>"+""<mul>|<sum>"-"<mul> */
{
  node *x = term();

  while (sym == PLUS || sym == MINUS || sym == STAR || sym == PERC || sym == SLASH)
    {
      node *t = x;
      switch(sym)
      {
        case STAR:  x = new_node(MULT); break;
        case PERC:  x = new_node(MOD);  break;
        case SLASH: x = new_node(DIV);  break;
        default: x = new_node(sym==PLUS ? ADD : SUB);
      }
      next_sym();
      x->o1 = t;
      x->o2 = mult();
    }

  return x;
}

node *mult() /* <mult> ::= <term>|<mult> "*" <term>|<mult> "/" <term>|<mult> "%" <term> */
{
  node *x = term();

  while(sym == STAR || sym == PERC || sym == SLASH)
  {
    node *t = x;
    switch(sym)
    {
      case STAR:  x = new_node(MULT); break;
      case PERC:  x = new_node(MOD);  break;
      case SLASH: x = new_node(DIV);  break;
    }
    next_sym();
    x->o1 = t;
    x->o2 = mult();
  }
  return x;
}

node *test() /* <test> ::= <sum> | <sum> "<" <sum>| <sum> "<=" <sum>| <sum> ">" <sum>
| <sum> ">=" <sum>| <sum> "==" <sum>| <sum> "!=" <sum> */
{
  node *x = sum();

  if (sym == LESS || sym == GREATER || sym == EXCLM)
    {
      node *t = x;
      switch(sym)
      {
        case LESS:    next_sym(); if (sym == EQUAL) {x = new_node(LTEQ); next_sym();}
                                  else x = new_node(LT);
                                  break;
        case GREATER: next_sym(); if (sym == EQUAL) {x = new_node(GTEQ); next_sym();}
                                  else x = new_node(GT);
                                  break;
        case EXCLM:   next_sym(); if(sym == EQUAL) { x = new_node(NOEQ); next_sym();}
                                  else syntax_error(4);
                                  break;
      }
      x->o1 = t;
      x->o2 = sum();
    }
  return x;
}

node *expr() /* <expr> ::= <test> | <id> "=" <expr> */
{
  node *x;

  x = test();

  if (sym == EQUAL)
    {
      node *t = x;
      next_sym();
      if(sym == EQUAL) //Test si il s'agit d'un test ou un assign
      {
        x = new_node(EQ);
        next_sym();
        x->o1 = t;
        x->o2 = sum();
      }
      else
      {
        x = new_node(ASSIGN);
        x->o1 = t;
        x->o2 = expr();
      }
    }
  return x;
}

node *paren_expr() /* <paren_expr> ::= "(" <expr> ")" */
{
  node *x;

  if (sym == LPAR) next_sym(); else syntax_error(7);

  x = expr();

  if (sym == RPAR) next_sym(); else syntax_error(6);

  return x;
}

node *statement()
{
  node *x;

  if (sym == IF_SYM)       /* "if" <paren_expr> <stat> */
    {
      x = new_node(IF1);
      next_sym();
      x->o1 = paren_expr();
      x->o2 = statement();
      if (sym == ELSE_SYM) /* ... "else" <stat> */
        { x->kind = IF2;
          next_sym();
          x->o3 = statement();
        }
    }
  else if (sym == WHILE_SYM) /* "while" <paren_expr> <stat> */
    {
      x = new_node(WHILE);
      next_sym();
      x->o1 = paren_expr();
      x->o2 = statement();
    }
  else if (sym == DO_SYM)  /* "do" <stat> "while" <paren_expr> ";" */
    {
      x = new_node(DO);
      next_sym();
      x->o1 = statement();
      if (sym == WHILE_SYM) next_sym(); else syntax_error(0);
      x->o2 = paren_expr();
      if (sym == SEMI) next_sym(); else syntax_error(0);
    }
  else if (sym == SEMI)    /* ";" */
    {
      x = new_node(EMPTY);
      next_sym();
    }
  else if (sym == LBRA)    /* "{" { <stat> } "}" */
    {
      x = new_node(EMPTY);
      next_sym();
      while (sym != RBRA)
        {
          node *t = x;
          x = new_node(SEQ);
          x->o1 = t;
          x->o2 = statement();
        }
      next_sym();
    }
  else if (sym == PRINT_SYM) /* "print" <paren_expr> */
  {
    x = new_node(PRINT);
    next_sym();
    x -> o1 = paren_expr();  //l'erreur de syntax est gere dans le cas de paren_expr
    if (sym == SEMI) next_sym(); else syntax_error(2);
  }

  else if (sym == GOTO_SYM) /*"goto" <id> ";"*/
  {
    x = new_node(GOTON);
    next_sym();
    x->o1 = expr();
    if (sym == SEMI) next_sym(); else syntax_error(2);
  }

  else if (sym == BRK_SYM) /*"break" [ <id> ] ";"*/
  {
    x = new_node(BREAK);
    next_sym();
    if (sym != SEMI) x->o1 = expr();
    else x->o1 = new_node(EMPTY);
    if (sym == SEMI) next_sym(); else syntax_error(2);
  }

  else if (sym == CON_SYM) /*"continue" [ <id> ] ";"*/
  {
    x = new_node(CONT);
    next_sym();
    if (sym != SEMI) x->o1 = expr();
    else x->o1 = new_node(EMPTY);
    if (sym == SEMI) next_sym(); else syntax_error(2);
  }

  else  /* <expr> ";" */
    {
      x = new_node(EXPR);
      x->o1 = expr();
      if (sym == COLON) //Repere et test les labels
      {
        x->kind = LABEL;
        next_sym();
        x->o2= statement();
      }
      else if (sym == SEMI) next_sym(); else syntax_error(2);
    }

  return x;
}

node *program()  /* <program> ::= <stat> */
{
  node *x = new_node(PROG);
  next_sym();
  x->o1 = statement();
  if (sym != EOI) syntax_error(1);
  return x;
}

/*---------------------------------------------------------------------------*/

/* Generateur de code. */
void compilation_error(int code)
{
  switch (code)
  {
  case 1: printf("Compilation Error: Label already assigned twice\n"); break;
  case 2: printf("Compilation Error: Jump to undeclared label\n"); break;
  case 3: printf("Compilation Error: Jump to label out of bounds\n"); break;
  case 4: printf("Compilation Error: Continue or break not nested in loop\n"); break;
  case 5: printf("Compilation Error: Continue or break with ID not in a nesting loop \n"); break;
  case 6: printf("Compilation Error: No memory available for compilation\n"); memory_dealloc(); exit(1);
  case 7: printf("Compilation Error: Maximum nested loops exceeded\n"); memory_dealloc(); exit(1);
  default: printf("Compilation Error:\n");
  }
  error = 1;
  return;
}

enum { ILOAD, ISTORE, BIPUSH, DUP, POP, IADD, ISUB, IMULT,
       IMOD, IDIV, GOTO, IFEQ, IFNE, IFLT, RETURN, IPRINT };

typedef signed char code;

code object[1000], *here = object;
code *jump[1000], next_jump = 0;
code *labels[26];
code *continu[250],  next_continu = 0; //il ne peut y avoir plus de boucle
code *brk[250], next_brk = 0; //meme chose que les continue
int names[26]; int num_name = 0; //utile pour la verification des loops
int current_loop = 27;

void gen(code c) {*here++ = c; if(here-object>1000) compilation_error(6);} //checks for overflow overflow
void assignLoopExit(code *start, code *end); /*foward declaration*/
void breaksAndContinues(code *jump, int next_stop, code *operator[]); /*Forward Declaration */

#ifdef SHOW_CODE
#define g(c) do { printf(" %d",c); gen(c); } while (0)
#define gi(c) do { printf("\n%s", #c); gen(c); } while (0)
#else
#define g(c) gen(c)
#define gi(c) gen(c)
#endif

void fix(code *src, code *dst)
{
  int jump = dst-src;
  if(!(jump > (-128) && jump < 127)) compilation_error(3); //Test pour saut trop grand
  *src = jump;
}


void c(node *x)
{ switch (x->kind)
    { case VAR   : gi(ILOAD); g(x->val); break;

      case CST   : gi(BIPUSH); g(x->val); break;

      case ADD   : c(x->o1); c(x->o2); gi(IADD);  break;

      case SUB   : c(x->o1); c(x->o2); gi(ISUB);  break;

      case MULT  : c(x->o1); c(x->o2); gi(IMULT); break;

      case MOD   : c(x->o1); c(x->o2); gi(IMOD);  break;

      case DIV   : c(x->o1); c(x->o2); gi(IDIV);  break;

      case LT    : gi(BIPUSH); g(1);
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case GT    : gi(BIPUSH); g(1);
                   c(x->o2);
                   c(x->o1);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

       case LTEQ : gi(BIPUSH); g(1);
                   c(x->o1);
                   c(x->o2);
                   gi(BIPUSH); g(1); //Puisqu'il s'agit d'entier la strategie fonctionne
                   gi(IADD);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

       case GTEQ : gi(BIPUSH); g(1);
                   c(x->o2);
                   c(x->o1);
                   gi(BIPUSH); g(1); //Puisqu'il s'agit d'entier la strategie fonctionne
                   gi(IADD);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case EQ    : gi(BIPUSH); g(1);
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFEQ); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0);break;

      case NOEQ  : gi(BIPUSH); g(1);
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFNE); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0);break;


      case ASSIGN: c(x->o2);
                   gi(DUP);
                   gi(ISTORE); g(x->o1->val); break;

      case IF1   : { code *p1;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2); fix(p1,here); break;
                   }

      case IF2   : { code *p1, *p2;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2);
                     gi(GOTO); p2 = here++; fix(p1,here);
                     c(x->o3); fix(p2,here); break;
                   }

      case WHILE : { code *p1 = here, *p2; current_loop++;
                     c(x->o1);
                     gi(IFEQ); p2 = here++;
                     c(x->o2);
                     gi(GOTO); fix(here++,p1); fix(p2,here);
                     assignLoopExit(p1, here); current_loop--; break;
                   }

      case DO    : { code *p1 = here; current_loop++; if(current_loop > 128) compilation_error(7); c(x->o1);
                     c(x->o2);
                     gi(IFNE); fix(here++,p1);
                     assignLoopExit(p1, here); current_loop--; break;
                   }

      case CONT  : {
                    if(current_loop == 27) compilation_error(4);
                    gi(GOTO);
                    if (x->o1->kind == EMPTY)
                    {
                      continu[next_continu] = here++;
                      *continu[next_continu++] = current_loop;
                    }
                    else
                    {
                     continu[next_continu] = here++;
                     *continu[next_continu++] = x->o1->val;
                    }
                    break;
                   }

      case BREAK : {
                    if(current_loop == 27) compilation_error(4);
                    gi(GOTO);
                    if (x->o1->kind == EMPTY)
                    {
                      brk[next_brk] = here++;
                      *brk[next_brk++] = current_loop;
                    }
                    else
                    {
                      brk[next_brk] = here++;
                      *brk[next_brk++] = x->o1->val;
                    }
                    break;
                   }
      case GOTON : {
                    gi(GOTO); jump[next_jump] = here++;
                    *jump[next_jump++] = x->o1->val; break;
                   }

      case LABEL : {
                     if(labels[x->o1->val] != NULL) compilation_error(1); //le label a deja ete utilise
                     labels[x->o1->val] = here;
                     c(x->o2); break;
                   }

      case EMPTY : break;

      case PRINT : c(x -> o1); gi(IPRINT); break;

      case SEQ   : c(x->o1);
                   c(x->o2); break;

      case EXPR  : c(x->o1);
                   gi(POP); break;

      case PROG  : c(x->o1);
                   gi(RETURN); break;
    }

}
/*Assingning the jump to goto, break and continue statement*/
void assignlabel()
{
  for(int i=0; i<next_jump; i++)
    {
      if (labels[*jump[i]] == NULL) compilation_error(2); //le label n'existe pas
      else fix(jump[i], labels[*jump[i]]);
    }
}


void assignLoopExit(code *start, code *end)
{
  /*Donne les etiquettes assigne a une loop*/
  num_name = 0;
  code *nameofloop = start;
  for(int i=0; i<26; i++) if(labels[i] == nameofloop) names[num_name++] = i;

  //Update les break
  breaksAndContinues(end, next_brk, brk);

  //Update les continues dans les boucles
  breaksAndContinues(start, next_continu, continu);

  // verifie que tout les continue et break on ete assigne a la fin de la boucle la plus englobante
  if(current_loop == 28)
  {
    int stop = next_brk > next_continu ? next_brk : next_continu;
    for (int i=0, j=0; i<stop; i++, j++)
    {
      if(brk[i] != NULL && i < next_brk) compilation_error(5);
      if(continu[j] != NULL && j < next_continu) compilation_error(5);
    }
  }
}
/*Methode qui s'occupe d'updater les breaks et les continues vers leurs boucles */
void breaksAndContinues(code *jump, int next_stop, code *operator[])
{
	//Verifie les operandes a updater dans les loops
	for(int i=0; i<next_stop; i++)
	{
		if(operator[i] == NULL) continue;
		//cas ou il n'y a pas d'etiquettes
		if(*operator[i] == current_loop)
		{
			fix(operator[i],jump);
			operator[i] = NULL;
		}
		//cas ou il y a une etiquette
		else
		{
			for (int j = 0; j<num_name; j++)
				if(*operator[i] == names[j])
				{
					fix(operator[i],jump);
					operator[i] = NULL;
          			break;
				}
		}
	}
}
/*---------------------------------------------------------------------------*/

/* Machine virtuelle. */

int globals[26];

void run()
{
  int stack[1000], *sp = stack; /* overflow? */
  code *pc = object;

  for (;;){
    switch (*pc++)
      {
        case ILOAD : *sp++ = globals[*pc++];             break;
        case ISTORE: globals[*pc++] = *--sp;             break;
        case BIPUSH: *sp++ = *pc++;                      break;
        case DUP   : sp++; sp[-1] = sp[-2];              break;
        case POP   : --sp;                               break;
        case IADD  : sp[-2] = sp[-2] + sp[-1]; --sp;     break;
        case ISUB  : sp[-2] = sp[-2] - sp[-1]; --sp;     break;
        case IMULT : sp[-2] = sp[-2] * sp[-1]; --sp;     break;
        case IMOD  : sp[-2] = sp[-2] % sp[-1]; --sp;     break;
        case IDIV  : sp[-2] = sp[-2] / sp[-1]; --sp;     break;
        case GOTO  : pc += *pc;                          break;
        case IFEQ  : if (*--sp==0) pc += *pc; else pc++; break;
        case IFNE  : if (*--sp!=0) pc += *pc; else pc++; break;
        case IFLT  : if (*--sp< 0) pc += *pc; else pc++; break;
        case IPRINT: printf("%d \n", sp[-1]); --sp;      break;
        case RETURN: return;
    }
    if(sp-stack > 1000)
    {
    	printf("Stack overflow\n");
    	memory_dealloc();
    	exit(1);
    }
  }
}

/*---------------------------------------------------------------------------*/

/* Programme principal. */

//Fonction qui dealloue la memoire de l'ASA.
void memory_deallocation(node *x)
{
	if(x->o1 == NULL && x->o2 == NULL && x->o3 == NULL) return;
	else
	{
		if(x->o1 != NULL){ memory_deallocation(x->o1); free(x->o1); }
		if(x->o2 != NULL){ memory_deallocation(x->o2); free(x->o2); }
		if(x->o3 != NULL){ memory_deallocation(x->o3); free(x->o3); }
    return;
	}
}

void memory_dealloc()
{
	memory_deallocation(root);
}

int main()
{
  int i;

  for (i=0; i<26; i++) //Initialise les valeurs des labels
    labels[i] = NULL;
  root = program();
  c(root);
  assignlabel();

#ifdef SHOW_CODE
  printf("\n");
#endif

  for (i=0; i<26; i++)
    globals[i] = 0;

  if(error == 0) run();
  memory_deallocation(root);

  // for (i=0; i<26; i++)
  //   if (globals[i] != 0)
  //     printf("%c = %d\n", 'a'+i, globals[i]);

  return 0;
}

/*---------------------------------------------------------------------------*/
