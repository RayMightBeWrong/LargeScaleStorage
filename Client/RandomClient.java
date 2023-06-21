import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class RandomClient {
    private String serverIP;
    private int serverPORT;
    private String name;
    private int interval;
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;
    private List<String> keys;

    public RandomClient(String ip, String port, String name, String interval){
        this.serverIP = ip;
        this.serverPORT = Integer.parseInt(port);
        this.name = name;
        this.interval = Integer.parseInt(interval);
        this.keys = new ArrayList<>();
    }

    public void run(){
        try {
            connectToServer(name, serverIP, serverPORT);
            System.out.println("CLIENT " + this.name + " INITIALIZED...\n");
            write(1, true);
            Random rand = new Random();
            while(true){
                Thread.sleep(interval);
                
                // decide next command
                int nextAction = rand.nextInt(2);
                if (nextAction == 0){
                    int requests_nr = rand.nextInt(2) + 1;
                    write(requests_nr, false);
                }
                else{
                    int requests_nr = Math.min(this.keys.size() - 1, rand.nextInt(5) + 1);
                    if (requests_nr > 0)
                        read(requests_nr);
                }
            }
        }
        catch (Exception e){
            e.printStackTrace();
        }
    }

    public void write(int nr, boolean initial) throws IOException{
        Random rand = new Random();
    
        if (initial){
            String key = generateRandomString(rand);
            String value = generateRandomString(rand);
            System.out.println("WRITING { key: " + key + " , value: " + value + " }");
            writeToServer(key, value);
            keys.add(key);
        }
        else{
            for (int i = 0; i < nr; i++){
                int genNewKey = rand.nextInt(11);
                String key, value;

                if (genNewKey <= 7){
                    key = pickExistingKey(rand);
                }
                else{
                    key = generateRandomString(rand);
                }

                value = generateRandomString(rand);
                System.out.println("WRITING { key: " + key + " , value: " + value + " }");
                writeToServer(key, value);
                
                if (genNewKey > 7)
                    keys.add(key);
            }
        }
        System.out.println("write request completed\n");
    }

    public void read(int nr) throws IOException{
        Random rand = new Random();
        List<String> l = new ArrayList<>();

        // picking keys to be read
        for (int i = 0; i < nr; i++){
            String key;
            key = pickExistingKey(rand);
            while (l.contains(key)){
                key = pickExistingKey(rand);
            }
            l.add(key);
        }

        System.out.println("READING " + nr + " KEYS");
        for (int i = 0; i < nr; i++){
            System.out.println("key" + (i+1) + " : " + l.get(i));
        }
        String answer = readFromServer(l);
        String[] kvs = answer.split(";");
        System.out.println("RESPONSE:");
        for (String kv: kvs){
            String[] elems = kv.split(":");
            System.out.println("{ key: " + elems[0] + " , value: " + elems[1] + " }");
        }
        System.out.println("reading request completed\n");
    }

    public String pickExistingKey(Random rand){
        int keygen = rand.nextInt(this.keys.size());
        return this.keys.get(keygen);
    }

    public String generateRandomString(Random rand){
        String randomKey = "";
        
        int len = rand.nextInt(10) + 1;
        for (int i = 0; i < len; i++)
            randomKey += generateRandomChar(rand);

        return randomKey;
    }

    public char generateRandomChar(Random rand){
        int _char = rand.nextInt(62);
        char c = '-';

        // 0  - 25 a-z
        if (_char < 26){
            c = (char)(_char + 'a');
        }
        // 26 - 51 A-Z
        else if (_char < 52){
            _char -= 26;
            c = (char)(_char + 'A');
        }
        // 52 - 61 0-9
        else{
            _char -= 52;
            c = (char)(_char + '0');
        }

        return c;
    }



    // session server communication functions

    public void writeToServer(String key, String value) throws IOException{
        out.write("write " + key + " " + value + "\n");
        out.flush();

        String res;
        while((res = in.readLine()) != null){
            if (res.equals("write_ack"))
                break;
        }
    }

    public String readFromServer(List<String> keys) throws IOException{
        StringBuilder sb = new StringBuilder("read ");
        for (int i = 0; i < keys.size() - 1; i++){
            sb.append(keys.get(i));
            sb.append(" ");
        }
        sb.append(keys.get(keys.size() - 1));
        sb.append("\n");
        out.write(sb.toString());
        out.flush();

        return in.readLine();
    }

    public void connectToServer(String name, String ip, int port) throws Exception{
        this.socket = new Socket(ip, port);
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new PrintWriter(socket.getOutputStream(), true);
        
        out.println("login " + name);
        out.flush();
    }

    public static void main(String[] args){
        // ip_server, port_server, id_cliente, intervalo entre pedidos
        if (args.length != 4){
            System.out.println("WRONG SYNTAX FOR RANDOM CLIENT APPLICATION!");
            System.out.println();
            System.out.println("CORRECT SYNTAX:");
            System.out.println("[arg1] session server ip");
            System.out.println("[arg2] session server port");
            System.out.println("[arg3] client name");
            System.out.println("[arg4] interval between end of request and start of another (milliseconds)");
        }
        else{
            new RandomClient(args[0], args[1], args[2], args[3]).run();
        }
    }
}
