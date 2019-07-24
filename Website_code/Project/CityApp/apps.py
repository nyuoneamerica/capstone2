from django.apps import AppConfig


class CityappConfig(AppConfig):
    name = 'CityApp'
def ready(self):
   # Makes sure all signal handlers are connected
  from . import handlers  # noqa