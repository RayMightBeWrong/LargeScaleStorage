import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
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
            PrintWriter out = new PrintWriter(client.getOutputStream(), true);

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
                        out.write("{" + args.get(i) + " : " + res.get(i) + "}");
                    }
                    out.write('\n');
                    out.flush();
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