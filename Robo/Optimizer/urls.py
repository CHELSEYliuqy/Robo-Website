from django.urls import path
from . import views


urlpatterns = [
    path('', views.home, name='home'),
    path('setup', views.setup, name='setup'),
    path('about', views.about, name='about'),
    path('real_record', views.real_record, name='real_record'),
    path('result', views.result, name='result'),
]
