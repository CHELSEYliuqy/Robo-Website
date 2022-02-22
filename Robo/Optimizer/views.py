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
    R_run_list = ['Rscript', os.path.join(root_path, 'bt_result.R')]
    port_base = ['ew', 'factor', 'rp', 'minv', 'maxser', 'maxser_h',
                 'maxser_slo', 'maxser_alo', 'maxser_h_slo', 'maxser_h_alo']

    param['cap'] = [request.POST['cap']]
    param['sigma'] = [request.POST['sigma']]
    param['mkt'] = [request.POST['Market']]
    param['reb_freq'] = [request.POST['rebf']]
    param['test_start'] = [request.POST['test_start']]
    param['test_end'] = [request.POST['test_end']]
    param['port'] = [p for p in port_base if p in request.POST.keys()]

    for p in param.keys():
        R_run_list = R_run_list + param[p]

    # run(R_run_list)
    #
    # result_path = os.path.join(root_path, 'bt_outcome', 'SSE50')
    # stat = pd.read_csv(os.path.join(result_path, 'stat.csv'))
    # cumret_all = pd.read_csv(os.path.join(result_path, 'cumret_all.csv'))
    # dd_all = pd.read_csv(os.path.join(result_path, 'drawdown_all.csv'))
    # latest_wts = pd.read_csv(os.path.join(result_path, 'MAXSER_ALO_latest_wts.csv'))
    # wts_summary = pd.read_csv(os.path.join(result_path, 'MAXSER_ALO_wts_summary.csv'))
    #
    # rtn = {}
    # dd = {}
    # smy = {}
    # for p in cumret_all.columns.to_list():
    #     if p == 'Date':
    #         rtn_dates = cumret_all[p].to_list()
    #         dd_dates = dd_all[p].to_list()
    #     else:
    #         rtn[p] = cumret_all[p].to_list()
    #         dd[p] = dd_all[p].to_list()
    #
    # stat_html = stat.to_html()
    # l_code = latest_wts.Code.to_list()
    # l_wts = latest_wts.Weigths.to_list()
    #
    # for w in wts_summary.columns.to_list():
    #     if w == 'Portfolio':
    #         tg_name = wts_summary.Portfolio.to_list()[1]
    #     elif w == 'Date':
    #         smy_dates = wts_summary.Date.to_list()
    #     else:
    #         smy[w] = wts_summary[w].to_list()
    #
    # context = {'stat': stat_html,
    #            'code': l_code, 'wt': l_wts,
    #            'rtn_dates': rtn_dates, 'rtn': rtn,
    #            'dd_dates': dd_dates, 'dd': dd,
    #            'smy_dates': smy_dates, 'smy': smy,
    #            'a': root_path,
    #            }

    context = {
               'a': root_path,
               }

    return render(request, 'result.html', context)
