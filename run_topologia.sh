#!/bin/bash

# Default values
default_data_servers=3
default_session_servers=3
default_data_brokers=2
default_session_brokers=2

# Initialize variables with default values
nr_data_servers=$default_data_servers
nr_session_servers=$default_session_servers
nr_data_brokers=$default_data_brokers
nr_session_brokers=$default_session_brokers

while getopts :d:s:b:r: opt;
do
    case "${opt}" in
        d) nr_data_servers=${OPTARG};;
        s) nr_session_servers=${OPTARG};;
        b) nr_data_brokers=${OPTARG};;
        r) nr_session_brokers=${OPTARG};;
        \?) echo "Invalid option ${OPTARG}"
    esac
done

echo "Number of data servers: $nr_data_servers"
echo "Number of session servers: $nr_session_servers"
echo "Number of data brokers: $nr_data_brokers"
echo "Number of session brokers: $nr_session_brokers"

data_brokers_start_port=8000
session_brokers_start_router_port=8100
session_brokers_start_pub_port=8200
session_servers_start_port=8300

data_brokers_ports=""
erlang_data_brokers=""
for ((i=0; i<nr_data_brokers; i++)); do
    port=$((data_brokers_start_port + i))
    data_brokers_ports+=" $port"
    if [ $i -gt 0 ]; then
        erlang_data_brokers+=","
    fi
    erlang_data_brokers+="{\"localhost\",$port}"
    java -cp .:./lib/jeromq-0.5.3.jar:./out/java Broker $port > /dev/null &
    echo "Started data broker $i on port $port"
done

session_brokers_router_ports=""
session_brokers_pub_ports=""
erlang_routers=""
erlang_pubs=""
for ((i=0; i<nr_session_brokers; i++)); do
    port_router=$((session_brokers_start_router_port + i))
    port_pub=$((session_brokers_start_pub_port + i))
    session_brokers_router_ports+=" $port_router"
    session_brokers_pub_ports+=" $port_pub"
    if [ $i -gt 0 ]; then
        erlang_routers+=","
        erlang_pubs+=","
    fi
    erlang_routers+="{\"localhost\",$port_router}"
    erlang_pubs+="{\"localhost\",$port_pub}"
    java -cp .:./lib/jeromq-0.5.3.jar:./out/java SessionServerBroker $port_router $port_pub > /dev/null &
    echo "Started session broker $i. Router port: $port_router ; Pub port: $port_pub"
done

# Execute custom commands for each number
for ((i=0; i<nr_data_servers; i++)); do
    java -cp .:./lib/jeromq-0.5.3.jar:./out/java DataServer $data_brokers_ports > /dev/null &
    echo "Started data server $i"
done

session_servers_ports=""
for ((i=0; i<nr_session_servers; i++)); do
    port=$((session_servers_start_port + i))
    session_servers_ports+=" $port"
    erl -pa ./_chumak/ebin/ ./out/erlang/ -eval "session_server:start_server($port, \"R$i\", [$erlang_routers], [$erlang_pubs], [$erlang_data_brokers])." -noshell > /dev/null &
    echo "Started session server $i"
done

echo "Session servers ports: $session_servers_ports"
