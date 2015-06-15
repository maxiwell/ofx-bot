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

class Teste(unittest.TestCase):

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
        self.myprofile.set_preference('browser.download.folderList', 0)
        self.myprofile.set_preference('browser.download.manager.showWhenStarting', False)
        self.myprofile.set_preference('browser.download.manager.focusWhenStarting', False)
        self.myprofile.set_preference('browser.download.useDownloadDir', True)
        self.myprofile.set_preference('browser.helperApps.alwaysAsk.force', False)
        self.myprofile.set_preference('browser.download.manager.alertOnEXEOpen', False)
        self.myprofile.set_preference('browser.download.manager.closeWhenDone', True)
        self.myprofile.set_preference('browser.download.manager.showAlertOnComplete', False)
        self.myprofile.set_preference('browser.download.manager.useWindow', False)
        self.myprofile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'multipart/form-data' )
        self.myprofile.update_preferences()

        self.driver = webdriver.Firefox(firefox_profile=self.myprofile)
        self.driver.implicitly_wait(30)
        self.base_url = "http://www.santander.com.br/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def test_e(self):

        driver = self.driver
        driver.get(self.base_url + "/")
        driver.find_element_by_xpath("//input[@name='txtCPF']").send_keys(self.cpf)
        driver.find_element_by_xpath("//input[@value='ok']").click()

        sleep(2)       
        handler = driver.window_handles[1]
        driver.close()
        driver.switch_to_window(handler)

        driver.switch_to_frame("Principal")
        driver.switch_to_frame("MainFrame")
        driver.find_element_by_css_selector("area[alt=\"Instalar mais tarde\"]").click()

        sleep(2) 
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
        driver.find_element_by_link_text(u"Extrato (Últimos 30 dias)").click()

        sleep(4)
        driver.switch_to_frame("iframePrinc")
        driver.find_element_by_link_text(u"exportar").click()
        driver.find_element_by_id("tipo3").click()
        driver.find_element_by_link_text("confirmar").click()

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
