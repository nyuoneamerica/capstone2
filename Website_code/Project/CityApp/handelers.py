# myapp/handlers.py
from corsheaders.signals import check_request_enabled

from .models import CityApp
from CityApp import models

def cors_allow_mysites(sender, request, **kwargs):
    return MySite.objects.filter(host=request.host).exists()

check_request_enabled.connect(cors_allow_mysites)