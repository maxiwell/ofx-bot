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
import getpass
import sys
import os

def ofxdriver():

    print "Entre com sua Agencia:"
    agencia = raw_input(); 
    print "Entre com sua Conta:"
    conta = raw_input();
    print "Entre com sua senha de 6 dígito (irei te roubar!): "
    if sys.stdin.isatty():
        password = getpass.getpass()
    else:
        # caso a senha seja redirecionada
        password = sys.stdin.readline().rstrip()

    print "Construindo o ambiente com Firefox..."
    myprofile = webdriver.FirefoxProfile()
    myprofile.set_preference('plugin.state.java', 2)
    myprofile.set_preference('browser.download.folderList', 2)
    myprofile.set_preference('browser.download.dir', os.getcwd())
    myprofile.set_preference('browser.download.manager.showWhenStarting', False)
    myprofile.set_preference('browser.download.manager.focusWhenStarting', False)
    myprofile.set_preference('browser.download.useDownloadDir', True)
    myprofile.set_preference('browser.helperApps.alwaysAsk.force', False)
    myprofile.set_preference('browser.download.manager.alertOnEXEOpen', False)
    myprofile.set_preference('browser.download.manager.closeWhenDone', True)
    myprofile.set_preference('browser.download.manager.showAlertOnComplete', False)
    myprofile.set_preference('browser.download.manager.useWindow', False)
    myprofile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'text/ofx,text/plain' )
    myprofile.update_preferences()

    driver = webdriver.Firefox(firefox_profile=myprofile)
    driver.implicitly_wait(30)
    base_url = "https://www2.bancobrasil.com.br/"
    verificationErrors = []
    accept_next_alert = True

def connecting():
    print "Conectando ao site do BB..."
    driver = driver
    driver.get(base_url + "aapf/login.jsp")
    driver.find_element_by_id("dependenciaOrigem").clear()
    driver.find_element_by_id("dependenciaOrigem").send_keys(agencia)
    driver.find_element_by_id("numeroContratoOrigem").clear()
    driver.find_element_by_id("numeroContratoOrigem").send_keys(conta)
    driver.find_element_by_id("senhaConta").clear()
    driver.find_element_by_id("senhaConta").send_keys(password)
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
    wait_and_find_element((By.XPATH,u"//a[@nome='Cartões']")).click()
    wait_and_find_element((By.XPATH,"//a[@codigo='3580']")).click()
    sleep(2)
    wait_and_find_element((By.XPATH,"//img[@title='PETROBRAS']")).click()
    wait_and_find_element((By.XPATH,"//a[@onclick='$.criarCaixaDialogoSalvarFatura(this,event);']")).click()
    wait_and_find_element((By.LINK_TEXT,"Money 2000+ (ofx)")).click()

    print "Acessando a Poupança..."

    # Baixar OFX da Poupanca
    wait_and_find_element((By.XPATH,u"//a[@nome='Poupança']")).click()
    wait_and_find_element((By.XPATH,"//a[@codigo='3909']")).click()
    wait_and_find_element((By.PARTIAL_LINK_TEXT,"30 dias")).click()
    wait_and_find_element((By.CSS_SELECTOR,"a.botaoToolBar.botaoToolBarSalvar")).click()
    wait_and_find_element((By.LINK_TEXT,"Money 2000+ (ofx)")).click()

    print "Roubando seu dinheiro..."
    sleep(4)

def is_element_present(how, what):
    try: driver.find_element(by=how, value=what)
    except NoSuchElementException, e: return False
    return True

def is_alert_present():
    try: driver.switch_to_alert()
    except NoAlertPresentException, e: return False
    return True

def close_alert_and_get_its_text():
    try:
        alert = driver.switch_to_alert()
        alert_text = alert.text
        if accept_next_alert:
            alert.accept()
        else:
            alert.dismiss()
        return alert_text
    finally: accept_next_alert = True

def tearDown():
    driver.quit()
    assertEqual([], verificationErrors)

def wait_and_find_element(by):
    delay = 30  # sec
    WebDriverWait(driver, delay).until(EC.visibility_of_element_located(by))
    return driver.find_element(*by)

if __name__ == "__main__":
    ofxdriver()
    connecting()
