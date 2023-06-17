import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.ZMsg;
import org.zeromq.ZFrame;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

public class DataServer{
    private String id;
    private ZContext ctx;
    private ZMQ.Socket socket;
    private DataMap data;

    public DataServer(List<String> ports, String id){
        try{
            this.ctx = new ZContext();
            this.socket = ctx.createSocket(SocketType.DEALER);
            this.id = id;
            socket.setIdentity(this.id.getBytes(ZMQ.CHARSET));
            for (String port: ports)
                socket.connect("tcp://localhost:" + port);
            socket.send("hello");

            this.data = new DataMap();
        }
        catch(Exception e){
            e.printStackTrace();
        }
    }

    public DataMap getDataMap(){ return this.data; }

    public void run(){
        while(true){
            try{
                ZMsg msg = ZMsg.recvMsg(socket);
                msg.pop();      // pop empty delimiter
                ZFrame content = msg.pop();

                System.out.println("received: " + content.toString() + "\n");
                handleMessage(content.toString());
            }
            catch(Exception e){
                e.printStackTrace();
            }
        }
    }

    public void handleMessage(String content) throws Exception {
        String[] command = content.split("!");
        DataServerMessage dsm = new DataServerMessage(command);

        if (dsm.getType().equals("write")){
            String request = dsm.getMessage();
            String[] k_v_deps = request.split(";");
            if (k_v_deps.length != 3)
                throw new Exception("Key-value write request with inproper format!");

            Map<String, Integer> deps = parseDeps(k_v_deps[2]);
            int new_version = this.data.write(k_v_deps[0], k_v_deps[1], deps);

            DataServerMessage newMsg = new DataServerMessage(true, dsm.getNodeID(), dsm.getClientID(), "write_ans", String.valueOf(new_version));
            this.socket.send(newMsg.constructMessage());
        }

        else if (dsm.getType().equals("read")){
            String request = dsm.getMessage();
            Map<String, String> answer = this.data.readOne(request);
            String res = request + ";" + answer.get("value") + ";" + answer.get("version") + ";" + answer.get("deps");

            DataServerMessage newMsg = new DataServerMessage(true, dsm.getNodeID(), dsm.getClientID(), "read_ans", res);
            this.socket.send(newMsg.constructMessage());
        }

        else if (dsm.getType().equals("read_version")){
            String request = dsm.getMessage();
            String[] k_version = request.split(";");
            String value = this.data.readVersion(k_version[0], k_version[1]);
            String res = k_version[0] + ";" + value + ";" + k_version[1];

            DataServerMessage newMsg = new DataServerMessage(true, dsm.getNodeID(), dsm.getClientID(), "read_version_ans", res);
            this.socket.send(newMsg.constructMessage());
        }
    }

    private Map<String, Integer> parseDeps(String deps) throws Exception{
        Map<String, Integer> res = new HashMap<>();
        
        // remove brackets
        StringBuilder info = new StringBuilder();
        for(int i = 1; i < deps.length() - 1; i ++)
            info.append(deps.charAt(i));
        deps = info.toString();
        
        if (!deps.equals("")){
            String[] elems = deps.split("/");
            for (String s: elems){
                String[] kv = parseDep(s);
                res.put(kv[0], Integer.parseInt(kv[1]));
            }
        }
        
        return res;
    }

    private String[] parseDep(String dep) throws Exception{
        // remove curly brackets
        StringBuilder info = new StringBuilder();
        for(int i = 1; i < dep.length() - 1; i ++)
            info.append(dep.charAt(i));
        dep = info.toString();
        
        String[] res = dep.split(",");
        if (res.length != 2)
            throw new Exception("Key-value write request with inproper format!");
        // force exception if version isn't an integer
        Integer.parseInt(res[1]);

        return res;
    }

    public static void main(String[] args){
        List<String> ports = new ArrayList<>();
        for (int i = 0; i < args.length; i++)
            ports.add(args[i]);

        // generate random hash
        Random rand = new Random();
        int rand1 = rand.nextInt(Integer.MAX_VALUE);
        double rand2 = rand.nextDouble();
        if (rand2 > 0.5)
            rand1 *= -1;
        String id = String.valueOf(rand1);

        DataServer server = new DataServer(ports, id);
        server.run();
    }
}