import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;

public class DataServer{
    private DataMap data;
    private Map<Integer, Integer> clock;
    private Reader reader;

    //TODO: tmp solution
    private int index;
    private Map<Integer, Integer> ports;
    private Map<Integer, Integer> serverHash;

    public DataServer(int index, Map<Integer, Integer> ports, Map<Integer, Integer> serverHash){
        this.data = new DataMap();
        this.clock = new HashMap<>();
        this.index = index;
        this.ports = ports;
        this.serverHash = serverHash;
        this.reader = new Reader();
    }

    public DataMap getDataMap(){ return this.data; }
    public Map<Integer, Integer> getClock(){ return this.clock; }
    public Reader getReader(){ return this.reader; }
    public int getIndex(){ return this.index; }
    public Map<Integer, Integer> getPorts(){ return this.ports; }
    public Map<Integer, Integer> getServerHashs(){ return this.serverHash; }

    public void run() throws Exception{
        ServerSocket server = new ServerSocket(ports.get(index));

        while(true){
            Socket client = server.accept();

            Thread thread = new Thread(new ClientHandler(client, this));
            thread.start();
        }
    }

    public static void main(String[] args){
        try {
            int index = Integer.parseInt(args[0]);
            // assigning servers statically
            // TODO: not that
            Map<Integer, Integer> ports = new HashMap<>();
            ports.put(1, 7001); ports.put(2, 7002); ports.put(3, 7003); ports.put(4, 7004); ports.put(5, 7005);

            Map<Integer, Integer> serverHash = new HashMap<>();
            serverHash.put(1, -1000000000); serverHash.put(2, -50000); serverHash.put(3, 50); 
            serverHash.put(4, 50000); serverHash.put(5, 1000000000);
            
            DataServer server = new DataServer(index, ports, serverHash);
            server.run();
        } 
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}