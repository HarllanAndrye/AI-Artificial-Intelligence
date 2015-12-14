# Baseado em:
# http://www.programeempython.com.br/blog/como-fazer-um-switch-com-python/
# http://www.vivaolinux.com.br/script/Calculo-do-dia-da-semana

import sys

# python diaSemana.py events_modify.csv events_modify_diaSemana.csv 
def main(args):
    
    #Define as funcoes
    def case_0(): return 'Segunda'
    def case_1(): return 'Terca'
    def case_2(): return 'Quarta'
    def case_3(): return 'Quinta'
    def case_4(): return 'Sexta'
    def case_5(): return 'Sabado'
    def case_6(): return 'Domingo'
    def case_default(): return 'Nao_existe'

    #cria um dicionario que relaciona cada funcao com a opcao desejada
    dict = {0 : case_0, 1 : case_1, 2 : case_2, 3 : case_3, 4 : case_4, 5 : case_5, 6 : case_6}

    inFilename = args[0]
    outFilename = args[1]
    inFile = open(inFilename,"r")
    lines = inFile.readlines()
    inFile.close()
    outFile = open(outFilename,"w")
    for line in lines:
        currentLine = line.split(",")
	r1 = currentLine[0].split(" ") # separa data e hora
	currentDate = r1[0].split("-") # separa dia, mes e ano
	r2 = r1[1].replace("'", "") # Time
	r2 = r2.split(":")
        r3 = currentLine[1] # Appliance
        r4 = currentLine[2] # on-off

        ano = int(currentDate[0].replace("'", ""))
	mes = int(currentDate[1])
	dia = int(currentDate[2])

	if mes == 2: dia += 31
	if mes == 3: dia += 59
	if mes == 4: dia += 90
	if mes == 5: dia += 120
	if mes == 6: dia += 151
	if mes == 7: dia += 181
	if mes == 8: dia += 212
	if mes == 9: dia += 243
	if mes == 10: dia += 273
	if mes == 11: dia += 304
	if mes == 12: dia += 334

	if ( ((ano%4)==0) & (((ano%100)!=0) | ((ano%400)==0)) & (mes > 2) ):
	    dia += 1
	dia += -1 + (ano-1)*365 + (ano-1)/4 -(ano-1)/100 +(ano-1)/400

	try:
	    diaSemana = dict[dia%7]()
	except:
	    diaSemana = case_default()

	# r2[0]: pega apenas a hora, ex.: 01, 14 ...
	# repr(x): server para colocar x entre aspas (quotedstr).
	outFile.write(",".join([str(diaSemana),str(r2[0]),str(r3),str(r4)]))
    outFile.close()

if __name__ == "__main__":
    main(sys.argv[1:])







