#!/usr/bin/env python2
# -*- coding: utf-8 -*-
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from time import sleep
import unittest, time, re
import getpass
import sys
import os

class Teste(unittest.TestCase):

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

        self.driver = webdriver.Firefox(firefox_profile=self.myprofile)
        self.driver.implicitly_wait(30)
        self.base_url = "https://www2.bancobrasil.com.br/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def test_e(self):


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
#        sleep(2)
        driver.find_element_by_partial_link_text("30 dias").click()
#        sleep(2)

        print "Baixando o OFX da Conta Corrente..."
 
        driver.find_element_by_css_selector("a.botaoToolBar.botaoToolBarSalvar").click()
        driver.find_element_by_link_text("Money 2000+ (ofx)").click()
#        sleep(2)

        print "Acessando o Cartao de Credito..."

        # Baixar OFX do Cartao Petrobras
        driver.find_element_by_xpath(u"//a[@nome='Cartões']").click()
        driver.find_element_by_xpath("//a[@codigo='3580']").click()
        driver.find_element_by_xpath("//img[@title='PETROBRAS']").click()
        print "Baixando o OFX do Cartao de Credito..."

        sleep(2)
        driver.find_element_by_xpath("//a[@onclick='$.criarCaixaDialogoSalvarFatura(this,event);']").click()
        driver.find_element_by_link_text("Money 2000+ (ofx)").click()

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

if __name__ == "__main__":
    unittest.main()
