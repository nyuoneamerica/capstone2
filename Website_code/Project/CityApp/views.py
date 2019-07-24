from django.shortcuts import render
from django.http import Http404
from rest_framework.views import APIView
from rest_framework.decorators import api_view
from rest_framework.response import Response
from rest_framework import status
from django.http import JsonResponse
from django.core import serializers
from django.conf import settings
import json
from rest_framework.decorators import action
from django.http import JsonResponse
from CityApp import models
import webbrowser
import subprocess
import cgi
import cgitb
import codecs
from urllib.request import urlopen
import urllib.parse as urlparse




# Create your views here.

@api_view(['GET'])
def main(self):
 # return JsonResponse(dict(geners=tuple("Give a valid Input/No Input(MSA) selected")))
 #return JsonResponse(dict(error=str("Give a valid Input/No Input(MSA) selected")))
  return render(self, 'Homepage.html')
#@action(detail=True)
@api_view(['GET'])
def getMsa(self):
 try:
      msaarr = []
      dbUtils = models.DbUtils()
      msaData = dbUtils.getMsa()
      #print(msaData)
      for r in msaData:    
        a = {"NAME": r[0]}
        msaarr.append(a)
        #print(msaarr)
      return Response(msaarr) 
    #   Response["Access-Control-Allow-Credentials"] = "true"
    #   Response["Access-Control-Allow-Origin"] = "*"
    #   Response["Access-Control-Allow-Methods"] = "GET, OPTIONS"
    #   Response["Access-Control-Max-Age"] = "86400"
    #   Response["Access-Control-Allow-Headers"] = "X-Requested-With, Content-Type"
      return Response
      #return JsonResponse(dict(geners=list(msaarr)), status=status.HTTP_200_OK)   
 except ValueError as e:
        return Response(e.args[0],status.HTTP_400_BAD_REQUEST)

@api_view(['GET'])
def msaDetails(request):
       # return JsonResponse(dict(error=str("Give a valid Input/No Input(MSA) selected")))
    try:
        dbUtils = models.DbUtils()
        if request.method == "GET":
                         msa = str(request.GET.get('NAME'))
                         type = request.GET.get('type', 'default')
                         #print (msa)
        else:
             return Response(str("Error: GET method Failed!!"))                 # render(request, 'demo/dashboard.html', context_dict)
        detailsData = dbUtils.getdetails(msa)
        #print(detailsData)
        for r in detailsData:
              data = r[0]
        return Response(data)
        # Response["Access-Control-Allow-Credentials"] = "true"
        # Response["Access-Control-Allow-Origin"] = "*"
        # Response["Access-Control-Allow-Methods"] = "GET, OPTIONS"
        # Response["Access-Control-Max-Age"] = "86400"
        # Response["Access-Control-Allow-Headers"] = "X-Requested-With, Content-Type"
        return Response
    except ValueError as e:
        return Response(e.args[0],status.HTTP_400_BAD_REQUEST)    

@api_view(['GET'])        
def neighborDetails(self):
    try:
        dbUtils = models.DbUtils()
        neighborData = dbUtils.getneighbordetails()
        for r in neighborData:
            data = r[0]
        return Response(data)
    except ValueError as e:
        return Response(e.args[0],status.HTTP_400_BAD_REQUEST)        
        
@api_view(['GET'])        
def topfivecitesbypopulation(self):        
        # return JsonResponse(dict(error=str("Give a valid Input/No Input(MSA) selected")))
    try:
        arr=[]
        dbUtils = models.DbUtils()        
        populationdata = dbUtils.gettopfivecitesbypopulation()
        for r in populationdata:
            data = r[0]
            arr.append(data)
            return Response(data)
    except ValueError as e:
        return Response(e.args[0],status.HTTP_400_BAD_REQUEST) 

@api_view(['GET'])  
def executekmeansmodel(request):
		#return JsonResponse(dict(error=str("Give a valid Input/No Input(MSA) selected")))
    try:
        data = str(request.GET.get('data'))
        m = json.loads(data)
		#print(m)
        msa = (m.get('MSA'))
		#Based on only 4 parms
        talent = (m.get('Talent'))
        connectivity = (m.get('Connectivity'))
        cost = (m.get('Cost'))
        quality = (m.get('Quality'))
	    # Define command and arguments
        command = 'Rscript'
        #path2script = 'D:/Virtual Directory/CityProject/CityApp/rscript1.R'
        path2script ='CityApp/KNN_4wt.R'
        #projects_json_fn = 'projects.json'
        #args = projects_json_fn  
        #arg = [str(msa),str(age),str(AvgProp),str(Bet10and14),str(Bet5and9),str(Bet65and74),str(LandArea),str(Net25and34),str(Under5),str(CorpTaxMin),str(CorpTaxMax),str(CO2Index),str(TotEnrollment),str(PercentBach),str(LaborForceAnnGrowth),str(PropPricetoIncomeRatio),str(CostLivingComposit),str(NumAirline),str(TrafficIndex),str(GDPperCap),str(GDP5Year),str(MedianIncome),str(CPIIndex)]
        arg = [str(msa),str(talent),str(connectivity),str(cost),str(quality)]
        #print(arg)Rscript --vanilla Knn_final_modified.r  sample.json New-York-City-NY 3.714881 2.286081 15.234578 15.657222 16.096229 4.023810 16.100002 15.657222 7.058431 2.880952 2.3142865 5 5 3.143557 5 5 5 5 5 5 5 3.825108
        cmd = [command,path2script]+arg 
        #+ msa + age + AvgProp+Bet5and9+Bet65and74+LandArea+Net25and34+Under5+CorpTaxMin+CorpTaxMax+CO2Index+TotEnrollment+PercentBach+LaborForceAnnGrowth+PropPricetoIncomeRatio+CostLivingComposit+NumAirline+TrafficIndex+GDPperCap+GDP5Year+MedianIncome+CPIIndex
        # check_output will run the command and store to result
        try:
            subprocess.check_output(cmd,universal_newlines=True,stderr=subprocess.STDOUT)
            #subprocess.check_output("dir /f",shell=True,stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as e:
            raise RuntimeError("command '{}' return with error (code {}): {}".format(e.cmd, e.returncode, e.output))
        
        #Hardcoded String
        # cities = {"MSA":"New York Newark Jersey City,NY NJ PA","First":"Chattanooga,TN GA","Second":"Canton Massillon,OH","Third":"Davenport Moline Rock Island,IA IL","Fourth":"Binghamton,NY","Fifth":"Columbia,SC","Sixth":"Chico,CA","Seventh":"Cleveland,TN","Eighth":"Cedar Rapids,IA","Ninth":"Des Moines West Des Moines,IA"}#subprocess.call(["/usr/bin/Rscript","--vanilla","D:/Virtual Directory/CityProject/CityApp/rscript1.R"])
		#subprocess.call(['Rscript','--vanilla', 'D:/Virtual Directory/CityProject/CityApp/knn_final.R',sample.json])	
        #subprocess.call(["""'Rscript','--vanilla','D:/Virtual Directory/CityProject/CityApp/knn_final.R'""",args])
          
        #The following code will create a html file and read a json file and display that content in a new browser tab
        # f = open('kmean.html','w')
        # message = """<html><head></head><body>
        #         <div>{}</div>
        #         </body></html>""".format(open(r"D:/Virtual Directory/CityProject/sample.json").read())
        # f.write(message)     
        # f.close() 
        # f=codecs.open("kmean.html", 'r')
        # webbrowser.get('chrome').open('kmean.html')
        # webbrowser.open_new_tab('kmean.html')
        # Below code opens a json file and parses it to print
        with open('/home/ubuntu/cityamerica_aws/Project/CityApp/output.json', 'r') as f:
            sample_dict = json.load(f)
        print(sample_dict)
        for city in sample_dict:
            print(list(city.values()))
            #print(list(city.keys()))
                
        #return Response(cities)
        #return JsonResponse(dict(Message=str("K-mean model run successfully!")))
            return Response(list(city.values()))
    except ValueError as e:
        return Response(e.args[0],status.HTTP_400_BAD_REQUEST)  		
		
		
#The below code is trial code if above code does not run the R code
												# arg1 = ["sample.json"]
												# cmd = [command, path2script] + arg1
                                                # if request.method == "GET":
                #path = dict(request.get_full_path())
                #print(path)
                #parsed = urlparse.urlparse(path)
                #print(urlparse.parse_qs(parsed.query)['MSA'])
                #print(path.MSA)
                # msa = request.GET['Age2034']
                # print(msa)
                # for key,value in data.items():
                #      print (key,values)
                #age = str(request.GET.get('Age2034')) 
         
                # print(msa)    
                # print(age)
                # print(AvgProp)
                # print(Bet5and9)
        # else:
        #      return Response(str("Error: GET method Failed!!"))    
											   # check_output will run the command and store to result
												#subprocess.call ("/usr/bin/Rscript --vanilla D:/Virtual Directory/CityProject/CityApp/rscript1.R",shell=True)
												#arg1 = "sample.json"
												#subprocess.call(['Rscript','--vanilla', 'D:/Virtual Directory/CityProject/CityApp/knn_final.R', arg1])
												#x = subprocess.check_output(cmd, universal_newlines=True)
												#process = subprocess.Popen(cmd,shell=True)
												#print('The maximum of the numbers is:', x)
											   # f = open('kmean.html','w')
												#message = """<html><head></head><body>
												 #       <div>{}</div>
												  #     </body></html>""".format(open(r"D:/Virtual Directory/CityProject/CityApp/sample.json").read())
												#f.write(message)     
												#f.close() 
												#f=codecs.open("kmean.html", 'r')
												#webbrowser.get('chrome').open('kmean.html')
												#webbrowser.open_new_tab('kmean.html')		

	
