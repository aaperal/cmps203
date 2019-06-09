"""
Classes for our AST nodes.
"""
class AST(object):
	pass

class ArithOp(AST):
	def __init__(self, izq, op, der):
		self.izq = izq
		self.token = self.op = op
		self.der = der

class ArithUnary(AST):
	def __init__(self, op, expr):
		self.token = self.op = op
		self.expr = expr

class Numero(AST):
	def __init__(self, token):
		self.token = token
		self.valor = token.valor

class Compuesta(AST):
	# Declaracion INICIO...FIN 
	def __init__(self):
		self.hijos = []

class Asignar(AST):
	def __init__(self, izq, op, der):
		self.izq = izq
		self.token = self.op = op
		self.der = der

class Var(AST):
	def __init__(self, token):
		self.token = token
		self.valor = token.valor

class SinOp(AST):
	pass

class Programa(AST):
	def __init__(self, nombre, bloque):
		self.nombre = nombre
		self.bloque = bloque

class Bloque(AST):
	def __init__(self, declaraciones, sentencia_compuesta):
		self.declaraciones = declaraciones
		self.sentencia_compuesta = sentencia_compuesta

class DeclaracionVar(AST):
	def __init__(self, nodo_variable, nodo_tipo):
		self.nodo_variable = nodo_variable
		self.nodo_tipo = nodo_tipo

class Tipo(AST):
	def __init__(self, token):
		self.token = token
		self.valor = token.valor


"""
Class for the tokens we generate. Each token has a type and a  value.
"""
class Token(object):
	def __init__(self, tipo, valor):

		self.tipo = tipo
		self.valor = valor

	def __str__(self):
		return 'Token({tipo}, {valor})'.format(
			tipo = self.tipo,
			valor = repr(self.valor)
		)

	def __repr__(self):
		return self.__str__()


"""
Class for our lexer. Our lexer takes in a piece of text (the program),
and has methods 
	siguiente_token - gets the next token in the file
	avanzar - does character by character step on the program
	saltar_espacios - skips whitespace
	error - returns an invalid character error
	saltar_comentarios - skips comments
	vistazo - peeks ahead to the next char, necessary when differentiating 
	between ':' and ':='.
	identificador - used for variables
	numero - used to construct number tokens
"""
class Lexer(object):
	def __init__(self, texto):
		self.texto = texto
		self.pos = 0
		self.simbolo_actual = self.texto[self.pos]

	def error(self):
		raise Exception('Simbolo inválido.')

	def avanzar(self):
		self.pos += 1
		if self.pos > len(self.texto) - 1:
			self.simbolo_actual = None #final de entrada
		else:
			self.simbolo_actual = self.texto[self.pos]

	def saltar_espacios(self):
		while self.simbolo_actual is not None and self.simbolo_actual.isspace():
			self.avanzar()

	def numero(self):
		resultado = ''
		while self.simbolo_actual is not None and self.simbolo_actual.isdigit():
			resultado += self.simbolo_actual
			self.avanzar()

		if self.simbolo_actual == '.':
			resultado += self.simbolo_actual
			self.avanzar()

			while self.simbolo_actual is not None and self.simbolo_actual.isdigit():
				resultado += self.simbolo_actual
				self.avanzar()

			token = Token('CONS_REAL', float(resultado))
		else:
			token = Token('CONS_ENT', int(resultado))

		return token

	def saltar_comentarios(self):
		while self.simbolo_actual != '}':
			self.avanzar()
		self.avanzar()

	def vistazo(self):
		peek_pos = self.pos + 1
		if peek_pos > len(self.texto) - 1:
			return None
		else:
			return self.texto[peek_pos]

	def identificador(self):
		resultado = ''
		while self.simbolo_actual is not None and self.simbolo_actual.isalnum():
			resultado += self.simbolo_actual
			self.avanzar()
		token = KEYWORDS.get(resultado.upper(), Token(ID, resultado))
		return token


	def siguiente_token(self):

		while self.simbolo_actual is not None:
			#print("sand")
			#print(self.simbolo_actual)
			#print("wich")


			if self.simbolo_actual.isspace():
				self.saltar_espacios()
				continue

			if self.simbolo_actual == '{':
				self.avanzar()
				self.saltar_comentarios()
				continue

			if self.simbolo_actual.isalpha():
				return self.identificador()

			if self.simbolo_actual.isdigit():
				return self.numero()

			if self.simbolo_actual == '+':
				self.avanzar()
				return 	Token(MAS, '+')

			if self.simbolo_actual == '-':
				self.avanzar()
				return Token(MENOS, '-')

			if self.simbolo_actual == '*':
				self.avanzar()
				return Token(MULT, '*')

			if self.simbolo_actual == '/':
				self.avanzar()
				return Token(DIV_FLOT, '/')

			if self.simbolo_actual == '(':
				self.avanzar()
				return Token(PI, '(')

			if self.simbolo_actual == ')':
				self.avanzar()
				return Token(PD, ')')

			if self.simbolo_actual == ':' and self.vistazo() == '=':
				self.avanzar()
				self.avanzar()
				return Token(ASIGNAR, ':=')

			if self.simbolo_actual == ';':
				self.avanzar()
				return Token(PYC, ';')

			if self.simbolo_actual == '.':
				self.avanzar()
				return Token(PUNTO, '.')

			if self.simbolo_actual == ':':
				self.avanzar()
				return Token(DOSPUNTOS, ':')

			if self.simbolo_actual == ',':
				self.avanzar()
				return Token(COMA, ',')


			self.error()

		return Token(FDA, None)


"""
The parser class, which takes in a lexer in the constructor. 
Has a method for every grammar rule as describe in the project report.
"""
class Parser(object):
	def __init__(self, lexer):
		self.lexer = lexer
		self.token_actual = self.lexer.siguiente_token()

	def error(self):
		raise Exception('Sintaxis inválida')

	def siguiente(self, tipo_token):

		if self.token_actual.tipo == tipo_token:
			print(self.token_actual)
			self.token_actual = self.lexer.siguiente_token()
		else:
			self.error()

	def programa(self):
		self.siguiente(PROGRAMA)
		nodo_var = self.variable()
		nombre_programa = nodo_var.valor
		self.siguiente(PYC)
		nodo_bloque = self.bloque()
		nodo_programa = Programa(nombre_programa, nodo_bloque)
		self.siguiente(PUNTO)
		return nodo_programa

	def bloque(self):
		nodos_decl = self.declaraciones()
		nodo_sent_compuesta = self.sentencia_compuesta()
		nodo = Bloque(nodos_decl, nodo_sent_compuesta)
		return nodo

	def declaraciones(self):
		decls = []
		if self.token_actual.tipo == VAR:
			self.siguiente(VAR)
			while self.token_actual.tipo == ID:
				decl_var = self.declaracion_variable()
				decls.extend(decl_var)
				self.siguiente(PYC)
		return decls

	def declaracion_variable(self):
		nodos_var = [Var(self.token_actual)]
		self.siguiente(ID)

		while self.token_actual.tipo == COMA:
			self.siguiente(COMA)
			nodos_var.append(Var(self.token_actual))
			self.siguiente(ID)

		self.siguiente(DOSPUNTOS)

		nodo_tipo = self.spec_tipo()
		dec_vars = [DeclaracionVar(nodo_var, nodo_tipo) for nodo_var in nodos_var]
		return dec_vars

	def spec_tipo(self):
		token = self.token_actual
		if self.token_actual.tipo == ENTERO:
			self.siguiente(ENTERO)
		else:
			self.siguiente(REAL)
		nodo = Tipo(token)
		return nodo

	def sentencia_compuesta(self):
		self.siguiente(INICIO)
		nodos = self.lista_sentencia()
		self.siguiente(FIN)

		raiz = Compuesta()
		for nodo in nodos:
			raiz.hijos.append(nodo)

		return raiz

	def lista_sentencia(self):
		nodo = self.sentencia()

		resultados = [nodo]

		while self.token_actual.tipo == PYC:
			self.siguiente(PYC)
			resultados.append(self.sentencia())

		return resultados

	def sentencia(self):
		if self.token_actual.tipo == INICIO:
			nodo = self.sentencia_compuesta()
		elif self.token_actual.tipo == ID:
			nodo = self.sentencia_asignacion()
		else:
			nodo = self.vacio()
		return nodo

	def sentencia_asignacion(self):
		izq = self.variable()
		token = self.token_actual
		self.siguiente(ASIGNAR)
		der = self.expr()
		nodo = Asignar(izq, token, der)
		return nodo

	def variable(self):
		nodo = Var(self.token_actual)
		self.siguiente(ID)
		return nodo

	def vacio(self):
		return SinOp()

	def factor(self):

		token = self.token_actual
		if token.tipo == MAS:
			self.siguiente(MAS)
			nodo = ArithUnary(token, self.factor())
			return nodo
		elif token.tipo == MENOS:
			self.siguiente(MENOS)
			nodo = ArithUnary(token, self.factor())
			return nodo
		elif token.tipo == CONS_ENT:
			self.siguiente(CONS_ENT)
			return Numero(token)
		elif token.tipo == CONS_REAL:
			self.siguiente(CONS_REAL)
			return Numero(token)
		elif token.tipo == PI:
			self.siguiente(PI)
			nodo = self.expr()
			self.siguiente(PD)
			return nodo
		else:
			nodo = self.variable()
			return nodo

	def term(self):

		nodo = self.factor()

		while self.token_actual.tipo in (MULT, DIV_FLOT, DIV_ENTERO):
			token = self.token_actual
			if token.tipo == MULT:
				self.siguiente(MULT)
			elif token.tipo == DIV_FLOT:
				self.siguiente(DIV_FLOT)
			elif token.tipo == DIV_ENTERO:
				self.siguiente(DIV_ENTERO)

			nodo = ArithOp(izq=nodo, op=token, der=self.factor())

		return nodo

	def expr(self):

		nodo = self.term()

		while self.token_actual.tipo in (MAS, MENOS):
			token = self.token_actual
			if token.tipo == MAS:
				self.siguiente(MAS)
			elif token.tipo == MENOS:
				self.siguiente(MENOS)

			nodo = ArithOp(izq=nodo, op=token, der=self.term())

		return nodo

	def parse(self):
		nodo = self.programa()
		if self.token_actual.tipo != FDA:
			self.error()

		return nodo

"""
NodeVisitor class based on the class of the same name in the ast Python module.
"""
class nodoVisitor(object):
	def visitar(self, nodo):
		method_name = 'visitar_' + type(nodo).__name__
		visita = getattr(self, method_name, self.visita_general)
		return visita(nodo)

	def visita_general(self, nodo):
		raise Exception('No hay metodo visitar_{}'.format(type(nodo).__name__))
"""
Interpreter class. Postorder visit of every node in the tree. 
"""
class Interpreter(nodoVisitor):
	def __init__(self, parser):
		self.parser = parser
		self.ambito_global = {}

	def visitar_ArithOp(self, nodo):
		if nodo.op.tipo == MAS:
			return self.visitar(nodo.izq) + self.visitar(nodo.der)
		elif nodo.op.tipo == MENOS:
			return self.visitar(nodo.izq) - self.visitar(nodo.der)
		elif nodo.op.tipo == MULT:
			return self.visitar(nodo.izq) * self.visitar(nodo.der)
		elif nodo.op.tipo == DIV_FLOT:
			return self.visitar(nodo.izq) / self.visitar(nodo.der)
		elif nodo.op.tipo == DIV_ENTERO:
			return self.visitar(nodo.izq) // self.visitar(nodo.der)

	def visitar_Numero(self, nodo):
		return nodo.valor

	def visitar_Programa(self, nodo):
		self.visitar(nodo.bloque)

	def visitar_Bloque(self, nodo):
		for dec in nodo.declaraciones:
			self.visitar(dec)
		self.visitar(nodo.sentencia_compuesta)

	def visitar_DeclaracionVar(self, nodo):
		pass

	def visitar_Tipo(self, nodo):
		pass

	def visitar_ArithUnary(self, nodo):
		if nodo.op.tipo == MAS:
			return +self.visitar(nodo.expr)
		elif nodo.op.tipo == MENOS:
			return -self.visitar(nodo.expr)

	def visitar_Compuesta(self, nodo):
		for hijo in nodo.hijos:
			self.visitar(hijo)

	def visitar_Asignar(self, nodo):
		self.ambito_global[nodo.izq.valor] = self.visitar(nodo.der)

	def visitar_Var(self, nodo):
		valor = self.ambito_global.get(nodo.valor)
		if valor is None:
			raise NameError(repr(nodo.valor))
		else:
			return valor

	def visitar_SinOp(self, nodo):
		pass

	def interpretar(self):
		tree = self.parser.parse()
		return self.visitar(tree)


# All tokens listed below.

# ENTERO - INTEGER
# MAS - PLUS
# FDA (Fin de archivo) - EOF (End of file)
ENTERO, MAS, FDA = 'ENTERO', 'MAS', 'FDA'
# MENOS - MINUS
MENOS = 'MENOS'
# mult - multiply
MULT = 'MULT'
#entre - divide
ENTRE = 'ENTRE'
# left parenthesis, right parenthesis
PI, PD = 'PI', 'PD'
KEYWORDS = {
	'INICIO': Token('INICIO', 'INICIO'),
	'FIN': Token('FIN', 'FIN'),
	'PROGRAMA': Token('PROGRAMA', 'PROGRAMA'),
	'VAR': Token('VAR', 'VAR'),
	'ENTRE': Token('DIV_ENTERO', 'ENTRE'),
	'ENTERO': Token('ENTERO', 'ENTERO'),
	'REAL': Token('REAL', 'REAL'),
}
PROGRAMA = 'PROGRAMA'
INICIO = 'INICIO'
FIN = 'FIN'
# asign, semi-colon
ASIGNAR = 'ASIGNAR'
PYC = 'PYC'
# period
PUNTO = 'PUNTO'
# id for variables
ID = 'ID'
# :
DOSPUNTOS = 'DOSPUNTOS'
# ,
COMA = 'COMA'
# real division
DIV_FLOT = 'ENTRE'
# int division
DIV_ENTERO = 'DIV_ENTERO'
VAR = 'VAR'
REAL = 'REAL'
# int and real constant
CONS_ENT = 'CONS_ENT'
CONS_REAL = 'CONS_REAL'



def main():
	texto = """
	PROGRAMA CMPS203;
	VAR
   		a, b : ENTERO;
   		c    : REAL;

	INICIO {CMPS203}
   		a := 47;
   		b := 47 ENTRE 4;
   		c := a * b / 2.5;
	FIN.  {CMPS203}
	"""

	lexer = Lexer(texto)
	parser = Parser(lexer)
	interpreter = Interpreter(parser)
	resultado = interpreter.interpretar()
	print(interpreter.ambito_global)



if __name__ == '__main__':
	main()

