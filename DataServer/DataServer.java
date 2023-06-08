import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.ZMsg;
import org.zeromq.ZFrame;

import java.util.ArrayList;
import java.util.List;

public class DataServer{
    private String id;
    private ZContext ctx;
    private ZMQ.Socket socket;
    private DataMap data;
    //private Map<Integer, Integer> clock;
    private Reader reader;

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
            //this.clock = new HashMap<>();
            this.reader = new Reader();
        }
        catch(Exception e){
            e.printStackTrace();
        }
    }

    public DataMap getDataMap(){ return this.data; }
    public Reader getReader(){ return this.reader; }

    public void run(){
        while(true){
            try{
                // TODO: receive noutra thread
                ZMsg msg = ZMsg.recvMsg(socket);
                // pop empty delimiter
                msg.pop();
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
            String[] kv_pair = request.split(",");
            if (kv_pair.length != 2)
                throw new Exception("Key-value requested to be written is not a pair!");
            this.data.write(kv_pair[0], kv_pair[1]);
        }
        
        else if (dsm.getType().equals("read")){
            String request = dsm.getMessage();
            String value = this.data.readOne(request);

            DataServerMessage newMsg = new DataServerMessage(true, dsm.getFrom(), "read", value);
            this.socket.send(newMsg.constructMessage());
        }
    }

    public static void main(String[] args){
        List<String> ports = new ArrayList<>();
        for (int i = 0; i < args.length - 1; i++)
            ports.add(args[i]);

        DataServer server = new DataServer(ports, args[args.length - 1]);
        server.run();
    }
}