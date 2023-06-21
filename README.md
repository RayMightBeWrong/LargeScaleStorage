# LargeScaleStorage

## CORRER O SISTEMA

### SERVIDORES DE DADOS

Compilar os ficheiros de servidores de dados dentro da diretoria DataServer com: ```./compile *.java```

###### BROKER

Para ligar o broker dos servidores de dados (p.ex. na porta 7000), usa-se: ```./run Broker 7000```

###### SERVIDORES DE DADOS

Para ligar um servidor de dados a um broker de dados (à escuta na porta 7000), faz-se: ```./run DataServer 7000```

### SERVIDORES DE SESSÃO

###### BROKER
Dentro da diretoria SessionServerBroker, compilar os ficheiros do broker dos servidores de sessão com: ```./compile *.java```
E pôe-se o servidor ativo com (p.ex. o ROUTER na porta 5555 e o PUB na porta 5556): ```./run Broker 5555 5556```

###### SERVIDORES DE SESSÃO
Dentro da diretoria SessionServer, compilar os ficheiros dos servidores de sessão com: 

```erlc -o out/ *.erl CRDTs/*.erl Actors/*.erl AuxiliaryModules/*.erl```

Inicia-se um runtime system de Erlang e um servidor de sessão (na porta 12345 e com id "R1") que se ligue aos brokers acima iniciados com:
```
erl -pa chumak/ebin/ out/
session_server:start_server(12345, "R1", [{"localhost",5555}], [{"localhost",5556}], [{"localhost",7000}]).
```

### CLIENTES

Compilar os ficheiros das aplicações cliente com: ```javac *.java```

###### CLIENTE INTERATIVO

A aplicação de um cliente interativo inicia-se com: ```java InteractiveClient```

###### CLIENTE AUTOMÁTICO

A aplicação de um cliente automatizado recebe como argumento: o IP do servidor de sessão, o port do servidor de sessão, o nome do cliente e o intervalo entre o fim de um pedido e o início de outro (em milissegundos).
Para um cliente automatizado que se ligue ao servidor de sessão criada em cima e que envie pedidos com um intervalo de 1 segundo: ```java InteractiveClient localhost 12345 client 1000```
