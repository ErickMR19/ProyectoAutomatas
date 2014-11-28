import ply.lex as lex
import ply.yacc as yacc

reservadas = {
    'INICIOPROGRAMA' : 'INICIOPROGRAMA',
    'FINPROGRAMA' : 'FINPROGRAMA',
    'PRINCIPAL' : 'PRINCIPAL',
    'LEER' : 'LEER',
    'IMPRIMIR' : 'IMPRIMIR',
    'PROCEDIMIENTO' : 'PROCEDIMIENTO',
    'INICIO' : 'INICIO',
    'FIN' : 'FIN',
    'ENTERO' : 'ENTERO',
    'CADENA' : 'CADENA',
    'REAL' : 'REAL',
    'CARACTER' : 'CARACTER',
    'SI' : 'SI',
    'ENTONCES' : 'ENTONCES',
    'SINO' : 'SINO',
    'MIENTRAS' : 'MIENTRAS',
    'HACER' : 'HACER',
    'REPETIR' : 'REPETIR',
    'HASTA' : 'HASTA',
    'NL' : 'NL',
    'VERDADERO' : 'VERDADERO',
    'FALSO' : 'FALSO',
    'Y' : 'Y',
    'O' : 'O'
}

tokens = ['PARRI', 'PARRD', 'PARCI', 'PARCD', 'COMA', 'IGUAL', 'MAS', 'MEN','POTENCIA', 'MUL', 'DIV', 'MOD', 'MAYQ', 'MENQ', 'NO', 'IGUIGU', 'MAYQIGU', 'MENQIGU', 'NOIGU', 'IDENTIFICADOR', 'LITENTERO', 'LITREAL', 'LITCADENA', 'LITCARACTER', 'FINLINEA'] + list(reservadas.values())

t_PARRI=r'\('
t_PARRD=r'\)'
t_PARCI=r'\['
t_PARCD=r'\]'
t_COMA=r','
t_IGUAL=r'='
t_MAS=r'\+'
t_MEN=r'-'
t_POTENCIA=r'\*\*'
t_MUL=r'\*'
t_DIV=r'/'
t_MAYQ=r'>'
t_MENQ=r'<'
t_NO=r'!'
t_IGUIGU=r'=='
t_MAYQIGU=r'>='
t_MENQIGU=r'<='
t_NOIGU=r'!='
t_LITENTERO=r'[0-9]+'
t_LITREAL=r'[0-9]+(\.[0-9]+)'
t_LITCADENA=r'"[A-Z0-9]+"'
t_LITCARACTER=r'[A-Z]'

def t_FINLINEA(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    return t

def t_IDENTIFICADOR(t):
    r'[A-Z][A-Z0-9]*'
    t.type = reservadas.get(t.value, 'IDENTIFICADOR')    # Check for reserved words
    print(t)
    return t


t_ignore = " \t"

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    print(t.value.count)
    exit(-1)
    t.lexer.skip(1)

# Build the lexer
lex.lex()


###### PARSER

listatokens = list()
listadeclaraciones = list()

def p_entrada(p):
    'statement : programa'
    print("VALIDO")

def p_programa(p):
    'programa : IDENTIFICADOR INICIOPROGRAMA cr decs-global-opc decs-proc-opc PRINCIPAL cr bloque FINPROGRAMA eof'

def p_cr(p):
    'cr : FINLINEA'

def p_eof(p):
    '''eof : cr
           | '''

########################################################## DECLARACIONES VARIABLES GLOBALES
def p_decs_global_opc(p):
    'decs-global-opc : decs-global'

def p_decs_global_opc_e(p):
    'decs-global-opc : '

def p_decs_global_M(p):
    'decs-global : decs-global dec-variable'

def p_decs_global(p):
    'decs-global : dec-variable'

########################################################## FIN DECLARACIONES VARAIBLES GLOBALES

def p_tipo_basico(p):
    '''tipo-basico : ENTERO
                    | REAL
                    | CADENA
                    | CARACTER '''

def p_LITERALBOOL(p):
    '''literal-bool : VERDADERO
                    | FALSO'''

def p_LITERAL(p):
    '''literal : LITENTERO
               | LITREAL
               | LITCADENA
               | LITCARACTER
               | literal-bool'''


########################################################## DECLARACIONES VARIABLES

def p_dec_variable_B(p):
    'dec-variable : tipo-basico decs-basico cr'

def p_dec_variable_A(p):
    'dec-variable : tipo-basico decs-arreglo cr'

#declaraciones variables simples

def p_decs_basico_M(p):
    'decs-basico : dec-basico COMA decs-basico'

def p_decs_basico(p):
    'decs-basico : dec-basico'

def p_dec_basico(p):
    'dec-basico  : IDENTIFICADOR '

def p_dec_basico_inic(p):
    'dec-basico : IDENTIFICADOR IGUAL VALORINICIALIZACON'

def p_VALORINICIALIZACON(p):
    '''VALORINICIALIZACON : literal
                        | IDENTIFICADOR '''

#declaraciones arreglos
def p_decs_arreglo_M(p):
    'decs-arreglo : dec-arreglo'

def p_decs_arreglo(p):
    'decs-arreglo : dec-arreglo COMA decs-arreglo'

def p_dec_arreglo(p):
    'dec-arreglo : IDENTIFICADOR PARCI tam-arreglo PARCD'

def p_dec_arreglo_bidimensional(p):
    'dec-arreglo : IDENTIFICADOR PARCI tam-arreglo PARCD PARCI tam-arreglo PARCD'

def p_tam_arreglo(p):
    '''tam-arreglo : IDENTIFICADOR
                   | LITENTERO '''
########################################################## FIN DECLARACIONES VARAIBLES

########################################################## DECLARACIONES INSTRUCCIONES

def p_inst_bloque(p):
    '''inst-bloque  : inst
					| bloque'''

def p_bloque(p):
    'bloque : INICIO cr bloque-inst-opc FIN cr'

def p_bloque_inst_opc_e(p):
    'bloque-inst-opc : '

def p_bloque_inst_opc(p):
    'bloque-inst-opc : bloque-inst'

def p_bloque_inst_M(p):
    'bloque-inst : inst bloque-inst'

def p_bloque_inst(p):
    'bloque-inst : inst'

def p_inst_declaracion(p):
    '''inst : dec-variable
            | inst-si-entonces
            | inst-si-entonces-sino
            | inst-mientras
            | inst-repetir
            | inst-imprimir
            | inst-leer
            | asignacion
            | exp cr'''

def p_asignacion(p):
    'asignacion	:	asignacionIDs exp cr'

def p_asignacion_ID(p):
    'asignacionIDs	:	IDENTIFICADOR IGUAL'

def p_asignacion_ID_M(p):
    'asignacionIDs	:	asignacionIDs IDENTIFICADOR IGUAL '

def p_inst_imprimir(p):
    'inst-imprimir : IMPRIMIR listaValores cr'

def p_inst_leer(p):
    'inst-leer : LEER listaID cr'

def p_lista_identificadores_M(p):
    'listaID : IDENTIFICADOR COMA listaID'

def p_lista_identificadores(p):
    'listaID  : IDENTIFICADOR '

def p_lista_valores_M(p):
    'listaValores : exp COMA listaValores'

def p_lista_valores(p):
    '''listaValores : exp
                    | NL'''
########################################################## FIN DECLARACIONES INSTRUCCIONES

########################################################## DECLARACIONES EXPRESIONES
precedence = (
	('left', 'Y'),
    ('left', 'O'),
	('left', 'MAYQ', 'MENQ','MAYQIGU','MENQIGU'),  # Nonassociative operators
	('left', 'IGUIGU','NOIGU'),
    ('left', 'MAS', 'MEN'),
    ('left', 'MUL', 'DIV','MOD'),
	('left', 'POTENCIA')
)

def p_expresiones(p):
    '''exp	: exp Y exp
            | exp O exp
            | exp MAYQ exp
            | exp MENQ exp
            | exp MAYQIGU exp
            | exp MENQIGU exp
            | exp IGUIGU exp
            | exp NOIGU exp
            | exp MAS exp
            | exp MEN exp
            | exp MUL exp
            | exp DIV exp
            | exp MOD exp
            | exp POTENCIA exp'''

def p_exp_parentesis(p):
    'exp	: PARRI exp PARRD'
    p[0] = p[2]

def p_exp_final(p):
    'exp	: exp-valor'

def p_exp_final_conoperador(p):
    'exp	: OPERADORUNARIO exp-valor'

def p_decs_globaswl(p):
    '''exp-valor	: literal
                    | IDENTIFICADOR'''


def p_operadoresunarios(p):
    '''OPERADORUNARIO   : NO
                        | MEN'''

########################################################## FIN DECLARACIONES EXPRESIONES

########################################################## DECLARACIONES CONDICIONALES
def p_inst_si_entonces(p):
    'inst-si-entonces : SI PARRI exp PARRD ENTONCES cr inst-bloque'

def p_inst_si_entonces_sino(p):
    'inst-si-entonces-sino : SI PARRI exp PARRD ENTONCES cr inst-bloque SINO inst-bloque'

def p_inst_mientras(p):
    'inst-mientras : MIENTRAS PARRI exp PARRD HACER cr inst-bloque'

def p_inst_repetir(p):
    'inst-repetir : REPETIR cr bloque-inst-opc HASTA PARRI exp PARRD cr'

########################################################## FIN DECLARACIONES CONDICIONALES
########################################################## DECLARACIONES PROCEDIMIENTOS
def p_decs_proc_opc_e(p):
    'decs-proc-opc : '

def p_decs_proc_opc(p):
    'decs-proc-opc : decs-proc'

def p_decs_proc_M(p):
    'decs-proc : decs-proc dec-proc'

def p_decs_proc(p):
    'decs-proc : dec-proc'

def p_dec_proc(p):
    'dec-proc : PROCEDIMIENTO IDENTIFICADOR cr bloque'


########################################################## FIN DECLARACIONES PROCEDIMIENTOS
def p_error(p):
    if p is not None:
        print("Error de sintaxis en la linea %s"%(p.lineno)+"::: %s inesperado" %p.type)
        yacc.errok()
    else:
        print("Fin de archivo inesperado")

yacc.yacc()

file = open('pruebas','r')
fileUpper = file.read().upper()
yacc.parse(fileUpper)
