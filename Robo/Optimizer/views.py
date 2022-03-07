from django.shortcuts import render
from django.http import HttpResponse
import sys
from subprocess import run, PIPE
import pandas as pd
import os


def home(request):
    return render(request, 'home.html', {'home_page': 'active'})


def setup(request):
    return render(request, 'setup.html', {'setup_page': 'active'})


def about(request):
    return render(request, 'about.html', {'about_page': 'active'})


def real_record(request):
    return render(request, 'real_record.html', {'real_record_page': 'active'})


def result(request):
    param = {}
    root_path = os.getcwd()
    R_run_list = ['Rscript', os.path.join(root_path, 'backtest', 'test.R')]
    # port_base = ['EW', 'Factor', 'Risk_Parity', 'Min_Var', 'MAXSER', 'MAXSER_H',
    #              'MAXSER_Stock_Long_Only', 'MAXSER_ALL_Long_Only', 'MAXSER_H_Stock_Long_Only', 'MAXSER_H_ALL_Long_Only']
    port_base = ['EW', 'Factor', 'MAXSER', 'MAXSER_H', 'MAXSER_Stock_Long_Only',
                 'MAXSER_ALL_Long_Only', 'MAXSER_H_Stock_Long_Only', 'MAXSER_H_ALL_Long_Only']

    param['cap'] = [request.POST['cap']]
    param['sigma'] = [request.POST['sigma']]
    param['mkt'] = [request.POST['Market']]
    param['reb_freq'] = [request.POST['rebf']]
    param['test_start'] = [request.POST['test_start']]
    param['test_end'] = [request.POST['test_end']]
    param['port'] = [p for p in port_base if p in request.POST.keys()]

    for p in param.keys():
        R_run_list = R_run_list + param[p]

    run(R_run_list)

    result_path = os.path.join(root_path, 'bt_outcome')
    stat = pd.read_csv(os.path.join(result_path, 'stat.csv'))
    cumret_all = pd.read_csv(os.path.join(result_path, 'cumret_all.csv'))
    dd_all = pd.read_csv(os.path.join(result_path, 'drawdown_all.csv'))

    latest_wts = {}
    wts_summary = {}
    for p in port_base:
        latest_wts[p] = pd.read_csv(os.path.join(result_path, p+'_latest_wts.csv'))
        wts_summary[p] = pd.read_csv(os.path.join(result_path, p+'_wts_summary.csv'))

    rtn = {}
    dd = {}
    for p in cumret_all.columns.to_list():
        if p == 'Date':
            rtn_dates = cumret_all[p].to_list()
            dd_dates = dd_all[p].to_list()
        else:
            rtn[p] = cumret_all[p].to_list()
            dd[p] = dd_all[p].to_list()

    stat_html = stat.to_html()

    l_code = {}
    l_wts = {}
    for k in latest_wts.keys():
        l_code[k] = latest_wts[k].Code.to_list()
        l_wts[k] = latest_wts[k].Weigths.to_list()

    smy_dates = {}
    smy_all = {}
    for k in wts_summary.keys():
        smy = {}
        for w in wts_summary[k].columns.to_list():
            if w == 'Portfolio':
                tg_name = wts_summary[k].Portfolio.to_list()[1]
            elif w == 'Date':
                smy_dates[k] = wts_summary[k].Date.to_list()
            else:
                smy[w] = wts_summary[k][w].to_list()
                smy_all[k] = smy

    context = {'stat': stat_html,
               'rtn_dates': rtn_dates, 'rtn': rtn,
               'dd_dates': dd_dates, 'dd': dd,
               }

    for t in port_base:
        context[t + '_code'] = l_code[t]
        context[t + '_wts'] = l_wts[t]
        context[t + '_smy_dates'] = smy_dates[t]
        context[t + '_smy'] = smy_all[t]
        if t in param['port']:
            context[t + '_flag'] = 1
        else:
            context[t + '_flag'] = 0


    return render(request, 'result.html', context)
