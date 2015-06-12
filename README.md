OFX-BOT
=============

Estes bots tem por objetivo automatizar a tarefa de conciliação bancária entre contas do Banco do Brasil,
Caixa Econômica e Santander com aplicativos que aceitam o formato 'ofx' (GNUCash, HomeBank.free.fr, Microsoft Money). 

#### ofx-bb:
* Baixa o 'ofx' da conta corrente
* Baixa o 'ofx' do cartão de crédito 'Petrobrás'

#### ofx-santander:
* Baixa o 'ofx' da conta corrente

#### ofx-caixa:
* Baixa o 'ofx' da conta corrente

Requisitos:
--------------

É necessário ter o pacote **selenium** do Python2:

```bash
pip2 install selenium
```

Como usar:
-------------

```bash
./ofx-bb.py

ou

./ofx-bb.py < input.cfg
```

Notas:
------------

Obviamente, este script supre minhas necessidades. Porém ele pode se tornar bem mais elaborado.
Alguns pontos a se melhorar futuramente:

* Criptografar a senha com a chave privada do usuário para evitar a digitação em todas execuções
* Opção de mais 'ofx' de acordo com o desejo do usuário (Poupança, outros cartões, ...)
* Mais tarefas automatizadas 

