# -*- coding: utf-8 -*-
"""
Module for displaying stock prices using
the Google Finance API.

Written and contributed by:
    Christian Brædstrup <christian AT fredborg-braedstrup.dk>
"""
import json

from time import time
try:
    # python 3
    from urllib.error import URLError
    from urllib.request import urlopen
except ImportError:
    # python 2
    from urllib2 import URLError
    from urllib2 import urlopen


class Py3status:
    """
    Configuration parameters:
        - cache_timeout: Should be at least 15 min according to bitcoincharts.
        - color_index  : Index of the market responsible for coloration,
                         meaning that the output is going to be green if the
                         price went up and red if it went down.
                         default: -1 means no coloration,
                         except when only one market is selected
        - field        : Field that is displayed per market,
                         see http://bitcoincharts.com/about/markets-api/
        - hide_on_error: Display empty response if True, else an error message
        - markets      : Comma-separated list of markets. Supported markets can
                         be found at http://bitcoincharts.com/markets/list/
        - symbols      : Try to match currency abbreviations to symbols,
                         e.g. USD -> $, EUR -> € and so on
    """

    # available configuration parameters
    cache_timeout = 60
    color_index = -1
    field = 'close'
    hide_on_error = False
    RollingStocks = False
    Tickers = "GOOG,AAPL"
    symbols = True
    
    def __init__(self):
        """
        Initialize last_price, set the currency mapping
        and the url containing the data.
        """
        self.currency_map = {
            'AUD': '$',
            'CNY': '¥',
            'EUR': '€',
            'GBP': '£',
            'USD': '$',
            'YEN': '¥'
        }

        self.last_price = 0
        self.current_stock = 0
        self.url = "http://finance.google.com/finance/info?client=ig&q="
        self.json_data = ''
        
    ''' 
    Google real-time data 
    Source:http://digitalpbk.com/stock/google-finance-get-stock-quote-realtime
    
    Single source url: http://finance.google.com/finance/info?client=ig&q=NASDAQ:GOOG
    Multiple sources url:  http://finance.google.com/finance/info?client=ig&q=NASDAQ:GOOG,NASDAQ:YHOO
    '''
    def _get_price(self, stock):
        """
        Given the data (in json format), returns the
        field for a given market.
        """
        return stock['l']

    def _get_procent(self, stock):
        """
        dgdf
        """
        return float(stock['cp'])

    def _print_rate(self,rate):
        """
        Print the rate pretty
        """
        frate = float(rate)
        if frate < 100:
            return rate
        else:
            return '{:.0f}'.format(frate)
                    
    def get_rate(self, i3s_output_list, i3s_config):
        response = {
            'cached_until': time() + self.cache_timeout,
            'full_text': ''
        }

        # get the data from the bitcoincharts website
        try:
            if (self.RollingStocks):
                s = self.Tickers.split(',')
                data = urlopen(self.url+s[self.current_stock]).read()
            else:
                data = urlopen(self.url+",".join(self.Tickers.split(','))).read()
            data = json.loads(data[3:])
            self.json_data = data
        except URLError:
            if not self.hide_on_error:
                response['color'] = i3s_config['color_bad']
                response['full_text'] = 'Stock charts unreachable'
            return response

        # get the rate for each market given
        rates, stocks = [], self.Tickers.split(',')
        color_rate = None
        for i, stock in enumerate(data):
#            stock = stock.strip()
            try:
                rate = self._get_price(stock)
                procent = self._get_procent(stock)
                # coloration
                if i == self.color_index or len(stock) == 1:
                    color_rate = rate
            except KeyError:
                continue
            # market name
            out = stock['t'] if rate else market
            out += ': '
            out += 'N/A' if not rate else self._print_rate(rate)
            out += 'N/A' if not rate else ' ({:.1f}%)'.format(procent)            
            rates.append(out)

        # only colorize if an index is given or
        # if only one market is selected
        if len(rates) == 1 or self.color_index > -1:
            if procent == 0.0:
                pass
            elif procent < 0.0:
                response['color'] = i3s_config['color_bad']
#        elif color_rate > self.last_price:
            elif procent > 0.0:
                response['color'] = i3s_config['color_good']
            self.last_price = color_rate
            
        response['full_text'] = ', '.join(rates)
        return response

if __name__ == '__main__':
    """
    Test this module by calling it directly.
    """
    from time import sleep
    x = Py3status()
    while True:
        print(x.get_rate([], {'color_good': 'green', 'color_bad': 'red'}))
        sleep(5)

