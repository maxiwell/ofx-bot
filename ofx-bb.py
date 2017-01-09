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
import subprocess, signal
import getpass
import sys
import os
import traceback

class BB():

    def startSelenium(self):
        print "Start Selenium"
        dirroot = os.path.dirname(os.path.realpath(sys.argv[0]))
        FNULL = open("/tmp/selenium.log", 'w')
        cmd  = 'java -jar ' + dirroot + '/selenium-server-standalone-3.0.1.jar'
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
            driver.find_element_by_xpath("//a[@title='Fechar']").click()
        except: pass 

        print "Acessando a Conta Corrente..."

        # Baixar OFX da Conta Corrente
        driver.find_element_by_css_selector("li.saldo-texto").click()
        driver.find_element_by_partial_link_text("30 dias").click()
        driver.find_element_by_css_selector("a.botaoToolBar.botaoToolBarSalvar").click()
        driver.find_element_by_link_text("Money 2000+ (ofx)").click()

        print "Acessando o Cartao de Credito..."

        # Baixar OFX do Cartao Petrobras
        self.wait_and_find_element((By.XPATH,u"//a[@nome='Cartões']")).click()
        self.wait_and_find_element((By.XPATH,"//a[@codigo='3580']")).click()
        sleep(2)
        self.wait_and_find_element((By.XPATH,"//img[@title='PETROBRAS']")).click()
        self.wait_and_find_element((By.XPATH,"//a[@onclick='$.criarCaixaDialogoSalvarFatura(this,event);']")).click()
        self.wait_and_find_element((By.LINK_TEXT,"Money 2000+ (ofx)")).click()

        print "Acessando a Poupança..."

        # Baixar OFX da Poupanca
        self.wait_and_find_element((By.XPATH,u"//a[@nome='Poupança']")).click()
        self.wait_and_find_element((By.XPATH,"//a[@codigo='3909']")).click()
        self.wait_and_find_element((By.PARTIAL_LINK_TEXT,"30 dias")).click()
        self.wait_and_find_element((By.CSS_SELECTOR,"a.botaoToolBar.botaoToolBarSalvar")).click()
        self.wait_and_find_element((By.LINK_TEXT,"Money 2000+ (ofx)")).click()

        print "Roubando seu dinheiro..."
        sleep(4)

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
   
    def is_alert_present(self):
        try: self.driver.switch_to_alert()
        except NoAlertPresentException, e: return False
        return True
    
    def close_alert_and_get_its_text(self):
        try:
            alert = self.driver.switch_to_alert()
            alert_text = alert.text
            if self.accept_next_alert:
                alert.accept()
            else:
                alert.dismiss()
            return alert_text
        finally: self.accept_next_alert = True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

    def wait_and_find_element(self, by):
        delay = 30  # sec
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

