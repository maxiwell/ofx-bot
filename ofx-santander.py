#!/usr/bin/env python2
# -*- coding: utf-8 -*-
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException

from time import sleep
import time, re
import subprocess, signal
import getpass
import sys, traceback
import os, urllib, tarfile, zipfile

class Santander():

    def startSelenium(self):
        dirroot = os.path.dirname(os.path.realpath(sys.argv[0]))
        seleniumlog = os.path.join(dirroot, "selenium.log")
        selenium = "selenium-server-standalone-3.0.1.jar"
        seleniumjar = os.path.join(dirroot, selenium)
        geckodriver = os.path.join(dirroot, 'geckodriver')

        print "Start Selenium (log: " + seleniumlog + ")"
        FNULL = open(seleniumlog, 'w')

        if not os.path.isfile(seleniumjar):
            urllib.urlretrieve('http://selenium-release.storage.googleapis.com/3.0/selenium-server-standalone-3.0.1.jar', seleniumjar)

        if not os.path.isfile(geckodriver):
            if sys.platform.startswith('linux'):
                urllib.urlretrieve('https://github.com/mozilla/geckodriver/releases/download/v0.13.0/geckodriver-v0.13.0-linux64.tar.gz', geckodriver + '.tar.gz')
                tar = tarfile.open(geckodriver + '.tar.gz')
                tar.extractall(dirroot)
                tar.close()
            elif sys.platform.startswith('win'):
                #FIXME: Not tested! I don't have windows.
                urllib.urlretrieve('https://github.com/mozilla/geckodriver/releases/download/v0.13.0/geckodriver-v0.13.0-win64.zip', geckodriver + '.zip')
                zf = zipfile.ZipFile(geckodriver + '.zip')
                zf.extractall(dirroot)
                zr.close()
            else:
                print 'Platform ' + sys.platform + ' not supported yet'
                sys.exit(1)
       
        cmd  = 'java -jar ' + seleniumjar
        os.environ["PATH"] += os.pathsep + dirroot
        
        #FIXME: Is it works on Windows? 
        self.process = subprocess.Popen(cmd, shell=True, stdout=FNULL, stderr=subprocess.STDOUT)
        sleep(3);

    def stopSelenium(self):
        print "Stop Selenium"
        self.driver.quit()

    def setUp(self):

        print "Entre com seu CPF:"
        self.cpf = raw_input(); 
        print "Entre com sua senha de 6 dígito (irei te roubar!): "
        if sys.stdin.isatty():
            self.password = getpass.getpass()
        else:
            # caso a senha seja redirecionada
            self.password = sys.stdin.readline().rstrip()

        print "Construindo o ambiente com Firefox..."
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
        self.myprofile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'multipart/form-data' )

        cap = webdriver.DesiredCapabilities.FIREFOX
        cap["marionette"] = True;
        self.driver = webdriver.Remote(
                        command_executor='http://127.0.0.1:4444/wd/hub',
                        browser_profile=self.myprofile,
                        desired_capabilities=cap)
        
        self.driver.implicitly_wait(30)
        self.driver.maximize_window()
        self.base_url = "http://www.santander.com.br/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def connecting(self):

        driver = self.driver
        driver.get(self.base_url + "/")
        driver.find_element_by_xpath("//input[@name='txtCPF']").send_keys(self.cpf)
        driver.find_element_by_xpath("//input[@value='ok']").click()

#        sleep(2)       
        try:
            handler = driver.window_handles[1]
            driver.close()
            driver.switch_to_window(handler)
        except:
            print "Versao com uma janela" 
        
        try:
            driver.switch_to_frame("Principal")
            driver.switch_to_frame("MainFrame")
            driver.find_element_by_css_selector("area[alt=\"Instalar mais tarde\"]").click()
        except: 
            print "Frame \"Instalar mais tarde\" não encontrado"

#        sleep(2) 
        driver.find_element_by_id("txtSenha").clear()
        driver.find_element_by_id("txtSenha").send_keys(self.password)
        driver.find_element_by_link_text("continuar").click()

        sleep(2)
        driver.switch_to_default_content()
        driver.switch_to_frame("Principal")
        driver.switch_to_frame("Menu")
        driver.find_element_by_partial_link_text("Corrente").click()

        sleep(2)
        driver.switch_to_default_content()
        driver.switch_to_frame("Principal")
        driver.switch_to_frame("Corpo")
        driver.find_element_by_partial_link_text(u"30 dias").click()

        sleep(1)
        driver.switch_to_frame("iframePrinc")
        sleep(2)
        driver.find_element_by_link_text("exportar").click()
        sleep(1)
        driver.find_element_by_id("tipo3").click()
        driver.find_element_by_link_text("confirmar").click()
        sleep(3)

def signal_handler(signal, frame):
    sys.exit(0)

if __name__ == "__main__":

    signal.signal(signal.SIGINT, signal_handler)
    
    santander = Santander()

    santander.startSelenium()
    try:
        santander.setUp()
        santander.connecting()
        santander.stopSelenium()
    except Exception as e:
        traceback.print_exc()
        santander.stopSelenium()

