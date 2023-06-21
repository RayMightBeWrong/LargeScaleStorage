import java.net.Socket;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class InteractiveClient {
    private String sessionServerIP;
    private int port;
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;
    
    public static void main(String[] args){
        new InteractiveClient().run();
    }

    public void run(){
        try{
            connect();
            clearScreen();
            int option = 0;
            do {
                option = mainMenu();
                switch (option){
                    case 1: 
                        clearScreen();
                        read();
                        break;

                    case 2: 
                        clearScreen();
                        write();
                        break;

                    case 3: 
                        clearScreen();
                        connect();
                        break;

                    case 4: 
                        option = -1;
                        break;
                }
                if (option != -1)
                    clearScreen();
            } while (option != -1);
            System.out.println("\nEXITING...");
        }
        catch (Exception e){
            e.printStackTrace();
        }
    }

    public int mainMenu(){
        Scanner sc = new Scanner(System.in);
        printBanner();
        System.out.println("\nCURRENTLY CONNECTED TO " + this.sessionServerIP + ":" + this.port);
        System.out.println("CHOOSE AN OPTION (1-4):");
        System.out.println("1. READ");
        System.out.println("2. WRITE");
        System.out.println("3. CONNECT TO DIFFERENT SERVER");
        System.out.println("4. EXIT");
        System.out.println("\nSELECT AN OPTION:");
        int option = sc.nextInt();

        return option;
    }
    
    public void connect() throws Exception{
        Scanner sc = new Scanner(System.in);
        printBanner();
        System.out.println("-------------------  CONNECT TO SERVER  -------------------");
        System.out.println("INSERT NAME: ");
        String name = sc.nextLine();
        System.out.println("\nAT IP: ");
        String ip = sc.nextLine();
        System.out.println("\nAT PORT: ");
        int port = sc.nextInt();
        System.out.println("\nCONNECTING TO " + this.sessionServerIP + ":" + this.port + "...");
        connectToServer(name, ip, port);
    }

    public void read() throws IOException{
        Scanner sc = new Scanner(System.in);
        printBanner();
        System.out.println("------------------ READING FROM DATABASE ------------------");
        System.out.println("HOW MANY VALUES TO READ: ");
        int nrKeys = sc.nextInt();

        List<String> keys = new ArrayList<>();
        for (int i = 1; i < nrKeys + 1; i++){
            System.out.println("\nKEY " + i + ": ");
            String value = sc.next();
            keys.add(value);
        }
        System.out.println();
        
        String answer = readFromServer(keys);
        String[] kvs = answer.split(";");
        System.out.println("RESPONSE:");
        for (String kv: kvs){
            String[] elems = kv.split(":");
            System.out.println("{ key: " + elems[0] + " , value: " + elems[1] + " }");
        }
        System.out.println();

        while(!answer.equals("y") && !answer.equals("Y")){
            System.out.println("DO YOU WISH TO CONTINUE (y)?");
            answer = sc.next();
        }
    }

    public void write() throws IOException{
        Scanner sc = new Scanner(System.in);
        printBanner();
        System.out.println("------------------- WRITING TO DATABASE -------------------");
        System.out.println("KEY: ");
        String key = sc.nextLine();
        System.out.println("\nVALUE: ");
        String value = sc.nextLine();
        writeToServer(key, value);
        System.out.println("WAITING FOR A RESPONSE...");
        String res;
        while((res = in.readLine()) != null){
            if (res.equals("write_ack"))
                break;
        }
    }

    public void connectToServer(String name, String ip, int port) throws Exception{
        this.sessionServerIP = ip;
        this.port = port;
        this.socket = new Socket(ip, port);
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new PrintWriter(socket.getOutputStream(), true);
        
        out.println("login " + name);
        out.flush();
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

    public void writeToServer(String key, String value){
        out.write("write " + key + " " + value + "\n");
        out.flush();
    }

    public static void clearScreen() {  
        System.out.print("\033[H\033[2J");  
        System.out.flush();  
    }

    public void printBanner(){
        System.out.println("=================== LARGE SCALE STORAGE ===================");
    }
}
