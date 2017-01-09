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
import time, re
import subprocess, signal
import getpass
import sys, traceback
import os, urllib, tarfile, zipfile

class BB():

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
        os.killpg(os.getpgid(self.process.pid), signal.SIGTERM) 

    def setUp(self):

        print "Entre com sua Agencia:"
        self.agencia = raw_input(); 
        print "Entre com sua Conta:"
        self.conta = raw_input();
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
        self.myprofile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'text/ofx,text/plain' )
        self.myprofile.update_preferences()


        cap = webdriver.DesiredCapabilities.FIREFOX
        cap["marionette"] = True;
        self.driver = webdriver.Remote(
                        command_executor='http://127.0.0.1:4444/wd/hub',
                        browser_profile=self.myprofile,
                        desired_capabilities=cap)

        self.driver.implicitly_wait(30)
        self.base_url = "https://www2.bancobrasil.com.br/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def connecting(self):


        print "Conectando ao site do BB..."
        driver = self.driver
        driver.get(self.base_url + "aapf/login.jsp")
        driver.find_element_by_id("dependenciaOrigem").clear()
        driver.find_element_by_id("dependenciaOrigem").send_keys(self.agencia)
        driver.find_element_by_id("numeroContratoOrigem").clear()
        driver.find_element_by_id("numeroContratoOrigem").send_keys(self.conta)
        driver.find_element_by_id("senhaConta").clear()
        driver.find_element_by_id("senhaConta").send_keys(self.password)
        driver.find_element_by_id("botaoEntrar").click()

        # Resolvendo janela de codigo
        try:
            self.wait_and_find_element((By.XPATH,"//a[@title='Fechar']"), 10).click()
        except: pass 

        print "Acessando a Conta Corrente..."

        # Baixar OFX da Conta Corrente
        self.wait_and_find_element((By.CSS_SELECTOR, "li.saldo-texto")).click()
        self.wait_and_find_element((By.PARTIAL_LINK_TEXT, "30 dias")).click()
        self.wait_and_find_element((By.CSS_SELECTOR, "a.botaoToolBar.botaoToolBarSalvar")).click()
        self.wait_and_find_element((By.LINK_TEXT, "Money 2000+ (ofx)")).click()
        sleep(1)

        print "Acessando o Cartao de Credito..."

        # Baixar OFX do Cartao Petrobras
        self.wait_and_find_element((By.XPATH,u"//li[@tipoextrato='2']")).click()
        sleep(1)
        self.wait_and_find_element((By.XPATH,"//img[@title='PETROBRAS']")).click()
        self.wait_and_find_element((By.XPATH,"//a[@onclick='$.criarCaixaDialogoSalvarFatura(this,event);']")).click()
        self.wait_and_find_element((By.LINK_TEXT,"Money 2000+ (ofx)")).click()

        print "Acessando a Poupança..."

        # Baixar OFX da Poupanca
        self.wait_and_find_element((By.XPATH,u"//li[@tipoextrato='3']")).click()
        while True: 
            try: 
                driver.find_element_by_partial_link_text("30 dias").click()
                break
            except:
                driver.find_element_by_xpath(u"//li[@class='ui-state-default ui-tabs-paging-next']").click()
                sleep(1)
        self.wait_and_find_element((By.CSS_SELECTOR,"a.botaoToolBar.botaoToolBarSalvar")).click()
        self.wait_and_find_element((By.LINK_TEXT,"Money 2000+ (ofx)")).click()

        print "Roubando seu dinheiro..."
        sleep(4)

    def wait_and_find_element(self, by, delay=30):
        WebDriverWait(self.driver, delay).until(EC.visibility_of_element_located(by))
        return self.driver.find_element(*by)


def signal_handler(signal, frame):
    sys.exit(0)

if __name__ == "__main__":

    signal.signal(signal.SIGINT, signal_handler)

    bb = BB()

    bb.startSelenium()
    try:
        bb.setUp()
        bb.connecting()
        bb.stopSelenium()
    except Exception as e:
        traceback.print_exc()
        bb.stopSelenium()

