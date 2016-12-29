#!/usr/bin/env python2
# -*- coding: utf-8 -*-
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException

from time import sleep
import unittest, time, re
import getpass
import sys
import os


class Teste(unittest.TestCase):

    def setUp(self):
        print "Entre com sua Conta:"
        self.conta = raw_input()
        
        print "Entre com sua senha de 6 dígito (irei te roubar!): "
        if sys.stdin.isatty():
            self.password = getpass.getpass()
        else:
            # caso a senha seja redirecionada
            self.password = sys.stdin.readline().rstrip()

        print "Launching Firefox ..."
        self.myprofile = webdriver.FirefoxProfile()
        self.myprofile.set_preference('plugin.state.java', 2)
        self.myprofile.set_preference('browser.download.folderList', 2)
        self.myprofile.set_preference('browser.download.dir', os.getcwd())
        self.myprofile.set_preference('browser.download.manager.showWhenStarting', False)
        self.myprofile.set_preference('browser.download.manager.focusWhenStarting', False)
        self.myprofile.set_preference('browser.download.useDownloadDir', True)
        self.myprofile.set_preference('browser.helperApps.alwaysAsk.force', False)
        self.myprofile.set_preference('browser.download.manager.alertOnEXEOpen', False)
        self.myprofile.set_preference('browser.download.manager.closeWhenDone', True)
        self.myprofile.set_preference('browser.download.manager.showAlertOnComplete', False)
        self.myprofile.set_preference('browser.download.manager.useWindow', False)
        self.myprofile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'text/ofx,text/plain' )
        self.myprofile.update_preferences()

        self.driver = webdriver.Firefox(firefox_profile=self.myprofile)
        self.driver.implicitly_wait(30)
        self.base_url = "https://www.creditmutuel.fr/fr/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def test_e(self):
        print "Authentification ..."
        driver = self.driver
        driver.get(self.base_url + "authentification.html")
        driver.find_element_by_id("_userid").clear()
        driver.find_element_by_id("_userid").send_keys(self.conta)
        driver.find_element_by_id("_pwduser").clear()
        driver.find_element_by_id("_pwduser").send_keys(self.password)
        driver.find_element_by_css_selector("input[name=submit]").click()
        
        sleep(5)
        print "Accessing download page ..."
        
        # Access to download page and select appropriate options:
        #   - OFX format
        #   - All accounts available
        driver.get(self.base_url + "banque/compte/telechargement.cgi")
        driver.find_element_by_id("ofx:DataEntry").click()
        driver.find_element_by_id("accountListController:chk").click()
        
        print "Downloading ..."
        driver.find_element_by_css_selector("input[name=_FID_DoDownload").click()
        
        sleep(4)
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)


if __name__ == "__main__":
    unittest.main()
