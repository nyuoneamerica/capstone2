from django.db import connection, models

from CityApp import views
from corsheaders.defaults import default_headers

CORS_ALLOW_HEADERS = default_headers + (
    'my-custom-header',
)
   #Create your models here.

class DbUtils(models.Model):
    def none(self):
        return self
    def getMsa(self):
          with connection.cursor() as cursor:
             cursor.execute("select \"NAME\" from citydetails.citydata")
             msaarr = cursor.fetchall()
             #print(msaarr)   
             return msaarr

    def getdetails(key,value):
            #value = dict(geners=list('Abilene,TX'))
            with connection.cursor() as cursor:
            #cursor.execute("select array_to_json(array_agg(t)) as \"MSA_Details\" from citydetails.citydetails as t where \"MSA\" = "  + "'" + value +"'" )
                 cursor.execute("select array_to_json(array_agg(t)) as \"MSA_Details\" from citydetails.citydata as t where \"NAME\" = " + "'" + value + "'")
                 #print(cursor)
                 detarr = cursor.fetchone()
                 #print(detarr)
                 return detarr
    def getneighbordetails(self):
         with connection.cursor() as cursor:
            cursor.execute("select array_to_json(array_agg(t)) as \"MSA_Nieghbor_Details\" from citydetails.msa_nearest_neighbors as t ")
            neighborDetailsaarr = cursor.fetchall()
            #print(neighborDetailsaarr)
            return neighborDetailsaarr        
    def gettopfivecitesbypopulation(self):
        with connection.cursor() as cursor:
            cursor.execute("select array_to_json(array_agg(t1)) FROM(select \"MSA\", \"Population\" from citydetails.citydata order by cast(\"Population\" as Integer) desc Limit 5) AS t1")        
            populationArr = cursor.fetchall()
            #print(populationArr)
            return populationArr; 
            
    
            
