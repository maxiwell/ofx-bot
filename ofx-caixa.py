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

        print "Entre com seu usuário caixa:"
        self.usuario = raw_input(); 
        print "Entre com sua senha:"
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
        self.myprofile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'text/ofx' )
        self.myprofile.accept_untrusted_certs = True
        self.myprofile.update_preferences()

        self.driver = webdriver.Firefox(firefox_profile=self.myprofile)
        self.driver.implicitly_wait(30)
        self.base_url = "https://internetbanking.caixa.gov.br/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def test_e(self):

        driver = self.driver
        driver.get(self.base_url + "SIIBC/index.processa")
        driver.find_element_by_id("usuario").clear()
        driver.find_element_by_id("usuario").send_keys(self.usuario)
        driver.find_element_by_name("tpPessoa").click()

        # esperando e escapando a tela de erro: 
        # "Falha na execução da biblioteca nativa ou ambiente não suportado"
        sleep(5)
        driver.switch_to_alert().accept()

        driver.find_element_by_css_selector("button.button-orange").click()
        driver.find_element_by_id("iniciais").click()
        
        sleep(3)
        driver.find_element_by_id("85SenhaAtual").clear()
        for i in range(0,len(self.password)):
            driver.find_element_by_xpath("//div[contains(@class, 'keyboard-button | pie') and text()='"+self.password[i]+"']").click()
        driver.find_element_by_id("85Confirm").click()
 
        sleep(5)

        #driver.find_element_by_xpath("//img[text()='Minha Conta']").click()
        driver.find_element_by_css_selector("img").click()
        driver.find_element_by_link_text("Extrato").click()
        sleep(5)
        driver.find_element_by_id("geraArquivoExtrato").click()
        driver.find_element_by_id("rdoFormatoArquivoOfx").click()
        driver.find_element_by_id("confirma").click()
        sleep(5)


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
