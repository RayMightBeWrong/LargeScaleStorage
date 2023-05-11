import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ClientHandler implements Runnable{
    private Socket client;
    private DataServer server;

    public ClientHandler(Socket socket, DataServer server){
        this.client = socket;
        this.server = server;
    }

    public void run(){
        try{
            BufferedReader in = new BufferedReader(new InputStreamReader(client.getInputStream()));
            PrintWriter out = new PrintWriter(client.getOutputStream(), true);

            String inputLine;
            while ((inputLine = in.readLine()) != null) {
                String[] command = inputLine.split(" ");
                System.out.println(inputLine);

                if (command[0].equals("SERVER_MSG")){
                    String from = in.readLine();
                    String clockStr = in.readLine();
                    String type = in.readLine();
                    String message = in.readLine();
                    DataServerMessage dsm = new DataServerMessage(from, clockStr, type, message);
                    System.out.println(dsm.getType() + " " + dsm.getMessage());
                    serverMessageHandler(dsm);
                }
                else if (command[0].equals("read")){
                    List<String> args = new ArrayList<>();
                    for (int i = 1; i < command.length; i++)
                        args.add(command[i]);

                    // TODO: add clock
                    int clock = 0;
                    server.getReader().createNewReadInstance(clock, args, out);
                    
                    for (int i = 0; i < args.size(); i++){
                        String key = args.get(i);
                        int hash = key.hashCode();
                        int closest = getClosest(hash);
                        if (closest == server.getIndex()){
                            String res = server.getDataMap().readOne(key);
                            boolean its_over = server.getReader().handleReadOk(clock, key, res);
                            if (its_over){
                                answerClientRead(out, server.getReader().getKVInstance(clock));
                            }
                        }
                        else {
                            DataServerMessage dsm = new DataServerMessage(Integer.toString(server.getIndex()), server.getClock(), "read", key);
                            Thread thread = new Thread(new QuickMessageSender(server.getPorts().get(closest), dsm));
                            thread.start();
                        }
                    }
                }
                else if (command[0].equals("write")){
                    int hash = command[1].hashCode();
                    int closest = getClosest(hash);
                    if (closest == server.getIndex()){
                        server.getDataMap().write(command[1], command[2]);
                    }
                    else {
                        DataServerMessage dsm = new DataServerMessage(Integer.toString(server.getIndex()), server.getClock(), "write", command[1] + " " + command[2]);
                        Thread thread = new Thread(new QuickMessageSender(server.getPorts().get(closest), dsm));
                        thread.start();
                    }
                }
            }
        }
        catch (Exception e){
            e.printStackTrace();
        }
    }

    public void serverMessageHandler(DataServerMessage dsm) throws Exception{
        if (dsm.getType().equals("read")){
            String key = dsm.getMessage();
            String value = server.getDataMap().readOne(key);

            // TODO: check if value was found
            DataServerMessage answer = new DataServerMessage(Integer.toString(server.getIndex()), server.getClock(), "read_ok", key + " " + value);
            Thread thread = new Thread(new QuickMessageSender(server.getPorts().get(Integer.parseInt(dsm.getFrom())) , answer));
            thread.start();
        }
        else if (dsm.getType().equals("write")){
            String content[] = dsm.getMessage().split(" ");
            String key = content[0];
            String value = content[1];

            server.getDataMap().write(key, value);
            DataServerMessage answer = new DataServerMessage(Integer.toString(server.getIndex()), server.getClock(), "write_ok", "");
            Thread thread = new Thread(new QuickMessageSender(server.getPorts().get(Integer.parseInt(dsm.getFrom())) , answer));
            thread.start();
        }
        else if (dsm.getType().equals("read_ok")){
            String content[] = dsm.getMessage().split(" ");
            String key = content[0];
            String value = content[1];

            System.out.println("key: " + key);
            System.out.println("value: " + value);

            // TODO: add clock
            int clock = 0;
            boolean its_over = server.getReader().handleReadOk(clock, key, value);
            if (its_over){
                PrintWriter output = server.getReader().getOutput(clock);
                Map<String, String> kv = server.getReader().getKVInstance(clock);
                answerClientRead(output, kv);
            }
        }
    }

    private int getClosest(int hashcode){
        int res = 0;
        int res_diff = 0;
        // if the key's hashcode is lesser than all of the servers' hashcodes
        boolean less = true;

        for (Map.Entry<Integer, Integer> entry: server.getServerHashs().entrySet()){
            int diff = hashcode - entry.getValue();
            
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

    public void answerClientRead(PrintWriter out, Map<String, String> kv){
        for (Map.Entry<String, String> entry: kv.entrySet())
            out.write("{" + entry.getKey() + " : " + entry.getValue() + "}");
        out.write('\n');
        out.flush();
    }

    class QuickMessageSender implements Runnable{
        private int portToSend;
        private DataServerMessage dsm;

        public QuickMessageSender(int portToSend, DataServerMessage dsm){
            this.portToSend = portToSend;
            this.dsm = dsm;
        }

        public void run(){
            // TODO: change ip later
            Socket socket;
            try {
                socket = new Socket("localhost", this.portToSend);
                PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
                out.print(this.dsm.constructMessage());
                out.flush();
                socket.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}