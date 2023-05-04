import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

public class ClientHandler implements Runnable{
    private Socket client; 
    private DataMap data;

    public ClientHandler(Socket socket, DataMap data){
        this.client = socket;
        this.data = data;
    }

    public void run(){
        try{
            BufferedReader in = new BufferedReader(new InputStreamReader(client.getInputStream()));
            String inputLine;
            while ((inputLine = in.readLine()) != null) {
                String[] command = inputLine.split(" ");
                System.out.println(inputLine);

                if (command[0].equals("read")){
                    List<String> args = new ArrayList<>();
                    for (int i = 1; i < command.length; i++)
                        args.add(command[i]);
                    
                    List<String> res = this.data.read(args);
                    for (int i = 0; i < args.size(); i++){
                        System.out.println("{" + args.get(i) + " : " + res.get(i) + "}");
                    }
                }
                else if (command[0].equals("write")){
                    this.data.write(command[1], command[2]);
                }
            }
        }
        catch (Exception e){
            e.printStackTrace();
        }
    }
}