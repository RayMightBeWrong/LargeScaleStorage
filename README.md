# LargeScaleStorage

##### LIGAR SERVIDOR DE SESSÃO
Iniciar na porta 7000 e ligar-se a um servidor de dados no IP 192.168.56.101 e porta 7001.
```
session:start(7000, {192,168,56,101}, 7001).
```

##### PEDIDOS POSSÍVEIS FEITOS PELO CLIENTE
```
read key1 key2 keyN
write key value
```
