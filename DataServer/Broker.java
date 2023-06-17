import java.util.HashMap;
import java.util.Map;

import org.zeromq.*;
import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Broker {
    private ZContext ctx;
    private ZMQ.Socket socket; 
    private Map<String, Integer> hashs;

    public Broker(String port){
        try{
            this.ctx = new ZContext();
            this.socket = ctx.createSocket(SocketType.ROUTER);
            socket.bind("tcp://*:" + port);

            this.hashs = new HashMap<>();
        }
        catch(Exception e){
            e.printStackTrace();
        }
    }

    public void mediate(){
        while(true){
            try{
                System.out.println("ready");
                ZMsg msg = ZMsg.recvMsg(socket);
                ZFrame sender = msg.pop();
                ZFrame content = msg.pop();
                System.out.println("sender: " + sender.toString());

                handleMessage(sender.toString(), content.toString());
                System.out.println();
            }
            catch(Exception e){
                e.printStackTrace();
            }
        }
    }

    public void handleMessage(String sender, String content) throws Exception {
        if (content.equals("hello_session")){
            // TODO: sync com outros proxies
        }
        else if (content.equals("hello")){
            acceptNewNode(sender.toString());
        }
        else{
            String[] command = content.split("!");
            for (int i=0; i < command.length; i++)
                System.out.println(i + ": " + command[i]);

            DataServerMessage dsm = new DataServerMessage(command);

            // handle messages from data servers
            if (dsm.getInner())
                handleInnerMessage(sender, dsm);

            // handle messages from session servers
            else
                handleOuterMessage(sender, dsm);
        }
    }

    public void handleInnerMessage(String sender, DataServerMessage dsm){
        if (dsm.getType().equals("read_ans")){
            DataServerMessage newMsg = new DataServerMessage(false, "", dsm.getClientID(), "read_ans", dsm.getMessage());
            sendMessage(dsm.getNodeID(), newMsg);
        }
        else if (dsm.getType().equals("write_ans")){
            DataServerMessage newMsg = new DataServerMessage(false, "", dsm.getClientID(), "write_ans", dsm.getMessage());
            sendMessage(dsm.getNodeID(), newMsg);
        }
        else if (dsm.getType().equals("read_version_ans")){
            DataServerMessage newMsg = new DataServerMessage(false, "", dsm.getClientID(), "read_version_ans", dsm.getMessage());
            sendMessage(dsm.getNodeID(), newMsg);
        }
    }

    public void handleOuterMessage(String sender, DataServerMessage dsm) throws Exception {
        if (dsm.getType().equals("write")){
            String[] k_v_deps = dsm.getMessage().split(";");
            if (k_v_deps.length != 3)
                throw new Exception("Key-value write request with inproper format!");
            String key = k_v_deps[0];

            DataServerMessage newMsg = new DataServerMessage(true, sender, dsm.getClientID(), "write", dsm.getMessage());
            int keyHash = key.hashCode();
            String to = getClosest(keyHash);
            sendMessage(to, newMsg);
        }
        else if (dsm.getType().equals("read")){
            String request = dsm.getMessage();
            String[] keys = request.split(";");

            for (String key: keys){
                DataServerMessage newMsg = new DataServerMessage(true, sender, dsm.getClientID(), "read", key);

                int keyHash = key.hashCode();
                String to = getClosest(keyHash);
                sendMessage(to, newMsg);
            }
        }
        else if (dsm.getType().equals("read_version")){
            String request = dsm.getMessage();
            String[] keys = request.split(";");

            if (keys.length == 2){
                DataServerMessage newMsg = new DataServerMessage(true, sender, dsm.getClientID(), "read_version", request);
            
                int keyHash = keys[0].hashCode();
                String to = getClosest(keyHash);
                sendMessage(to, newMsg);
            }
            else
                throw new Exception("Key-version requested is not a pair!");
        }
    }

    public void acceptNewNode(String id){
        this.hashs.put(id, 0);
    }

    private String getClosest(int hashcode) throws Exception {
        String res = "";
        int res_diff = Integer.MIN_VALUE;
        // if the key's hashcode is smaller than all of the servers' hashcodes
        boolean less = true;

        for (Map.Entry<String, Integer> entry: this.hashs.entrySet()){
            int nodeHash = Integer.parseInt(entry.getKey());
            int diff = hashcode - nodeHash;
            
            if (less && diff > 0){
                less = false;
                res = entry.getKey();
                res_diff = diff;
            }
            // if less is true => res_diff < 0
            // if diff > res_diff then it is closer to the key's hashcode in the chord 
            else if (less && diff > res_diff){
                res = entry.getKey();
                res_diff = diff;
            }
            else if (less == false && diff >= 0 && diff < res_diff){
                res = entry.getKey();
                res_diff = diff;
            }
        }

        return res;
    }

    public void sendMessage(String to, DataServerMessage dsm){
        ZFrame identity = new ZFrame(to);
        ZMsg newMsg = new ZMsg();
        newMsg.wrap(identity);
        newMsg.add(dsm.constructMessage());
        System.out.println("sending to: " + to);
        System.out.println("sending: " + dsm.constructMessage());
        newMsg.send(socket);
    }

    public static void main(String[] args) {
        Broker b = new Broker(args[0]);
        b.mediate();
    }
}


