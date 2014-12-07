import ply.lex as lex
import ply.yacc as yacc
import sys

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

tokens = ['PARRI', 'PARRD', 'PARCI', 'PARCD', 'COMA', 'IGUAL', 'MAS', 'MEN','POTENCIA', 'MUL', 'DIV', 'MOD', 'MAYQ', 'MENQ', 'NO', 'IGUIGU', 'MAYQIGU', 'MENQIGU', 'NOIGU', 'IDENTIFICADOR', 'LITENTERO', 'LITREAL', 'LITCADENA', 'LITCARACTER', 'FINDELINEA'] + list(reservadas.values())

t_PARRI=r'\('
t_PARRD=r'\)'
t_PARCI=r'\['
t_PARCD=r'\]'
t_COMA=r','
t_IGUAL=r'='
t_MAS=r'\+'
t_MEN=r'-'
t_MOD=r'%'
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
t_LITCADENA=r'"[A-Z0-9\.\-\s]+"'
t_LITCARACTER=r'\'[A-Z]\''

def t_LITREAL(t):
    r'[0-9]+(\.[0-9]+)'
    t.value = float(t.value)
    return t

def t_LITENTERO(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_FINDELINEA(t):
    r'(\n)+'
    t.lexer.lineno += t.value.count("\n")
    return t

def t_ignore_comment(t):
    r'(  (/\*(.|\n)*?\*/) |(//.*?\n) )+'
    t.lexer.lineno += t.value.count("\n")

def t_IDENTIFICADOR(t):
    r'[A-Z][A-Z0-9]*'
    t.type = reservadas.get(t.value, 'IDENTIFICADOR')    # Check for reserved words
    return t


t_ignore = " \t"

#reconoce e ignora los comentarios
#basada de la encontrada en ply/cpp.py
def t_ignore_comentarios(t):
    r'(/\*(.|\n)*?\*/)|(//.*?\n)'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    print(t.value.count)
    exit(-1)
    t.lexer.skip(1)


###### PARSER

def p_entrada(p):
    'statement : programa'

# se elimino el identificador del programa al considerarse innecesario
def p_programa(p):
    'programa : INICIOPROGRAMA cr decs-global-opc decs-proc-opc PRINCIPAL cr bloque FINPROGRAMA eof'

def p_cr(p):
    'cr : FINLINEA'

def p_eof(p):
    '''eof : cr
           | '''

# el token findelinea reconoce n cantidad de lineas, sin embargo se esta produccion se agrega porque al encontrar caracteres
# que se ignoran, como los espacios y los comentarios, se pueden contar estos tokens dos o mas veces
def p_finlinea(p):
    '''FINLINEA : FINDELINEA FINLINEA
                | FINDELINEA'''

########################################################## DECLARACIONES VARIABLES GLOBALES
def p_decs_global_opc(p):
    'decs-global-opc : decs-global'
    p[0] = list(p[1])

def p_decs_global_opc_e(p):
    'decs-global-opc : '

def p_decs_global_M(p):
    'decs-global : decs-global dec-variable'
    listaTemporalExterna.insert(0,p[2])
    p[0] = listaTemporalExterna

def p_decs_global(p):
    'decs-global : dec-variable'
    listaTemporalExterna.insert(0,p[1])
    p[0] = listaTemporalExterna

########################################################## FIN DECLARACIONES VARAIBLES GLOBALES

#tipos basicos de las variables
def p_tipo_basico(p):
    '''tipo-basico : ENTERO
                    | REAL
                    | CADENA
                    | CARACTER '''
    p[0] = p[1]

def p_LITERALBOOL(p):
    '''literal-bool : VERDADERO
                    | FALSO'''

#los distintos tipos de literales
def p_LITERAL(p):
    '''literal : LITENTERO
               | LITREAL
               | LITCADENA
               | LITCARACTER
               | literal-bool'''
    p[0] = p[1]

########################################################## DECLARACIONES VARIABLES
# declaracion de variables sencillas
def p_dec_variable_B(p):
    'dec-variable : tipo-basico decs-basico cr'
    p[0] = ('declaraciones simple',p[1],list(p[2]))
    #lo guarda en un diccionario para mantener variables y tipos
    for v in p[2]:
        diccionariosVariablesGlobales[v[1]] = p[1]
    listaTemporal.clear()

# declaracion de variables que son arreglos
def p_dec_variable_A(p):
    'dec-variable : tipo-basico decs-arreglo cr'
    p[0] = ('declaraciones arreglo',p[1],list(p[2]))
    #lo guarda en un diccionario para mantener variables y tipos
    for v in p[2]:
        if v[0] == 'arreglou':
            diccionariosVariablesGlobales[v[1]] = p[1]+"[]"
        else:
            diccionariosVariablesGlobales[v[1]] = p[1]+"[][]"
    listaTemporal.clear()

#declaraciones variables simples

def p_decs_basico_M(p):
    'decs-basico : dec-basico COMA decs-basico'
    listaTemporal.insert(0,p[1])
    p[0] = listaTemporal


def p_decs_basico(p):
    'decs-basico : dec-basico'
    listaTemporal.insert(0,p[1])
    p[0] = listaTemporal

def p_dec_basico(p):
    'dec-basico  : IDENTIFICADOR '
    # verifica que el identificador no haya sido utilizado
    if p[1] in listaIdentificadores:
        listaDeErroresSemanticos.append("Identificador duplicado: "+p[1]+" en la linea: "+str(p.lineno(1) ) +":%s"%(find_column_production(fileUpper,p,1)) )
    listaIdentificadores.append(p[1])
    p[0] = ('No inicializada',p[1])

def p_dec_basico_inic(p):
    'dec-basico : IDENTIFICADOR IGUAL VALORINICIALIZACON'
    # verifica que el identificador no haya sido utilizado
    if p[1] in listaIdentificadores:
        listaDeErroresSemanticos.append("Identificador duplicado: "+p[1]+" en la linea: "+str(p.lineno(1) ) +":%s"%(find_column_production(fileUpper,p,1)) )
    listaIdentificadores.append(p[1])
    p[0] = ('inicializada',p[1],p[3])

def p_VALORINICIALIZACON(p):
    '''VALORINICIALIZACON : literal
                        | IDENTIFICADOR '''
    p[0] = p[1]

#declaraciones arreglos
def p_decs_arreglo_M(p):
    'decs-arreglo : dec-arreglo'
    listaTemporal.insert(0,p[1])
    p[0] = listaTemporal

def p_decs_arreglo(p):
    'decs-arreglo : dec-arreglo COMA decs-arreglo'
    listaTemporal.insert(0, p[1])
    p[0] = listaTemporal

def p_dec_arreglo(p):
    'dec-arreglo : IDENTIFICADOR PARCI tam-arreglo PARCD'
    # verifica que el identificador no haya sido utilizado
    if p[1] in listaIdentificadores:
        listaDeErroresSemanticos.append("Identificador duplicado: "+p[1]+" en la linea: "+str(p.lineno(1) ) +":%s"%(find_column_production(fileUpper,p,1)) )
    listaIdentificadores.insert(0,p[1])
    p[0] = ('arreglou',p[1],p[3])

def p_dec_arreglo_bidimensional(p):
    'dec-arreglo : IDENTIFICADOR PARCI tam-arreglo PARCD PARCI tam-arreglo PARCD'
    # verifica que el identificador no haya sido utilizado
    if p[1] in listaIdentificadores:
        listaDeErroresSemanticos.append("Identificador duplicado: "+p[1]+" en la linea: "+str(p.lineno(1) ) +":%s"%(find_column_production(fileUpper,p,1)) )
    listaIdentificadores.insert(0,p[1])
    p[0] = ('arreglob',p[1],p[3],p[6])

def p_tam_arreglo(p):
    '''tam-arreglo : IDENTIFICADOR
                   | LITENTERO '''
    if type(p[1]) != type(1): #si es un identificador
        if p[1] not in listaIdentificadores:
            # verifica que el identificador haya sido utilizado
            listaDeErroresSemanticos.append("Identificador no declarado: "+p[1])
        else:
            try:
                if diccionariosVariablesGlobales[p[1]] != 'ENTERO':
                    listaDeErroresSemanticos.append("Identificador: "+p[1]+" no valido en este ambito, el tamaño de los arreglos deben declararse con un entero. ("+str(p.lineno(1) ) +":%s"%(find_column_production(fileUpper,p,1))+")")
            except KeyError:
                    listaDeErroresSemanticos.append("Identificador: "+p[1]+" no puede utilizarse para declarar un arreglo, si este fue declarado en la misma sentencia. ("+str(p.lineno(1) ) +":%s"%(find_column_production(fileUpper,p,1))+")")


    p[0] = p[1]
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
    # verifica que el identificador haya sido utilizado
    if p[1] not in listaIdentificadores:
        listaDeErroresSemanticos.append("Identificador no declarado: "+p[1])

def p_lista_identificadores(p):
    'listaID  : IDENTIFICADOR '
    # verifica que el identificador haya sido utilizado
    if p[1] not in listaIdentificadores:
        listaDeErroresSemanticos.append("Identificador no declarado: "+p[1])

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
    p[0] = p[1]

def p_exp_final_conoperador(p):
    'exp	: OPERADORUNARIO exp-valor'

def p_decs_valores(p):
    'exp-valor	: literal'
    p[0] = p[1]

def p_decs_valores_id(p):
    'exp-valor	: IDENTIFICADOR'
    # verifica que el identificador haya sido utilizado
    if p[1] not in listaIdentificadores:
        listaDeErrores.append("Identificador no declarado: "+p[1])


def p_operadoresunarios(p):
    '''OPERADORUNARIO   : NO
                        | MEN'''

########################################################## FIN DECLARACIONES EXPRESIONES

########################################################## DECLARACIONES CONDICIONALES
def p_inst_si_entonces(p):
    'inst-si-entonces : SI PARRI exp PARRD ENTONCES cr inst-bloque'

def p_inst_si_entonces_sino(p):
    'inst-si-entonces-sino : SI PARRI exp PARRD ENTONCES cr inst-bloque SINO cr inst-bloque'

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
    # verifica que el identificador no haya sido utilizado
    if p[2] in listaIdentificadores:
        listaDeErroresSemanticos.append("Identificador duplicado: "+p[2]+" en la linea: "+str(p.lineno(2) ) +":%s"%(find_column_production(fileUpper,p,2)) )
    else:
        listaProcedimientos.append(p[2])
        listaIdentificadores.insert(0,p[2])

########################################################## FIN DECLARACIONES PROCEDIMIENTOS
def p_error(p):
    if p is not None:
        listaDeErrores.append("Error de sintaxis en la linea %s"%(p.lineno)+":%s"%(find_column(fileUpper,p))+" :: %s inesperado" %p.type)
        yacc.errok()
    else:
        listaDeErrores.append("Fin de archivo inesperado")

# TOMADA DE http://www.dabeaz.com/ply/ply.html
# Compute column.
#     input is the input text string
#     token is a token instance
def find_column(input,token):
    last_cr = input.rfind('\n',0,token.lexpos)
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos - last_cr)
    return column

# Similar a la anterior, es util cuando se quiere indicar la posición de un error no lexico/sintactico
def find_column_production(input,token,entrada):
    last_cr = input.rfind('\n',0,token.lexpos(entrada))
    if last_cr < 0:
        last_cr = 0
    column = (token.lexpos(entrada) - last_cr)
    return column

yacc.yacc()

# obtiene los argumentos de invocacion
archivos = list(sys.argv)
#se quita el propio nombre
archivos.remove(sys.argv[0])
archivos.append("pruebas")
if len(archivos) == 0:
        print("error, debe enviar el nombre de los archivos deseados como parametro de invocacion\npython[3] "+sys.argv[0]+" archivo [archivo2 ... archivoN]")
for filename in archivos:
    # Build the lexer
    lex.lex()
    listaDeErrores = list()
    listaDeErroresSemanticos = list()
    listaIdentificadores = list()
    diccionariosVariablesGlobales = dict()
    listaProcedimientos = list()
    listaTemporal = list()
    listaTemporalExterna = list()
    try:
        file = open(filename,'r')
    except FileNotFoundError:
        print("Archivo no encontrado: ",filename)
        continue

    print("////////////////////\nArchivo: ",filename)

    fileUpper = file.read().upper()
    yacc.parse(fileUpper)


    for item in listaDeErrores:
        print(item)

    if len(listaDeErrores) > 0:
        print("////////////////////")
        continue

    for item in listaDeErroresSemanticos:
        print(item)

    if len(listaDeErroresSemanticos) > 0:
        print("////////////////////")
        continue

    print("Identificadores de variables y su tipo")
    print(diccionariosVariablesGlobales)
    print("Identificadores de procedimientos")
    print(listaProcedimientos)

    print("El archivo es valido.\n////////////////////")