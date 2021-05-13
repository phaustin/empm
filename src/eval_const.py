#
#
#
# data pi43/4.1887902047863905/,eps/0.622/,rd/287./ 
# data cw/4190./,rhow/1000./,cv/1875./
# data g/9.8/,Rv/461.5/,cpd/1.0057e3/,epstv/0.608/ 
# data aedens/1.774e3/,salden/2.165e3/,con2/0.2856858/
# data c2am/0.4695335/,alpha/1./,beta/0.04/
# data c2nacl/0.616564/
# data c7am/0.4363021/
# data c7nacl/0.5381062/
import pprint
pp=pprint.PrettyPrinter(indent=4)

constants=dict()
constants['ambi']=dict(dens=1.774e3,mw=115.11,vh=2)
constants['nacl']=dict(dens=2.165e3,mw=58.44,vh=2)
constants['amm']=dict(dens=1.78e3,mw=132.14,vh=3)
# c
# c molecular weights in kg/Kmole
# c
amsufm=132.14
ambim=115.11
naclm=58.44
waterm=18.016
output=dict()
for the_key in constants:
    values=constants[the_key]
    constants[the_key]['c2']=values['vh']*waterm/values['mw']
    constants[the_key]['c7'] = (values['dens'] - 1.e3)/values['dens']

pp.pprint(constants)
