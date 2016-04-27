def creaPacientes():
	strResult = "(list%s)"
	strPacientes = ""
	while(input("Comenzamos") != '*'):
		nombre = input()
		apellido = input()
		horas = input()
		horasString = ""
		for i in horas.split(','):
			horasString += " " + str(i)
		strPacientes += " (list \"" + nombre + "\" \"" + apellido +"\" (list" + horasString + "))"
	print(strResult%strPacientes)
