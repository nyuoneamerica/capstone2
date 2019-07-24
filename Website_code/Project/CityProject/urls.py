"""CityProject URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/2.1/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
from django.contrib import admin
from django.urls import path,re_path
from django.conf.urls import url
from CityApp import views
from CityApp import cors
from django.views.generic import TemplateView
from django.conf import settings
from django.conf.urls.static import static
from django.contrib.staticfiles.urls import static
from django.contrib.staticfiles.urls import staticfiles_urlpatterns


urlpatterns = staticfiles_urlpatterns()
urlpatterns = static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
urlpatterns = static(settings.STATIC_URL, document_root=settings.STATIC_ROOT)
urlpatterns = [
    path('admin/', admin.site.urls),
    path('',views.main),
    path('getmsa',views.getMsa),
    #url(r'^msadetails/(?P<MSA>)/$', views.msaDetails, name = 'msadetails'),
    #re_path(r'^msadetails/$',views.msaDetails),
    path('msadetails',views.msaDetails),
    path('neighbordetails',views.neighborDetails),
    path('topfivecitesbypopulation',views.topfivecitesbypopulation),
  	path('home',TemplateView.as_view(template_name = 'Homepage.html')),
    path('Homepage2.html',TemplateView.as_view(template_name = 'Homepage2.html')),
    path('details.html',TemplateView.as_view(template_name = 'details.html')),
    path('charts.html',TemplateView.as_view(template_name = 'charts.html')),
	path('NewHome',TemplateView.as_view(template_name = 'Home.html')),
	path('KMeanscreen',TemplateView.as_view(template_name = 'KMean home screen.html')),
    path('source',TemplateView.as_view(template_name = 'DataSources.html')),
    path('analytics',TemplateView.as_view(template_name = 'Analytics.html')),
    path('kmeansmodel',views.executekmeansmodel)
] + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)

