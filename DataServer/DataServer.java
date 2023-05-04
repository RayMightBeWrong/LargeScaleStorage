import java.net.ServerSocket;
import java.net.Socket;

public class DataServer{
    private static int PORT = 7001;

    public static void main(String[] args){
        try {
            ServerSocket server = new ServerSocket(PORT);
            DataMap data = new DataMap();

            while(true){
                Socket client = server.accept();

                Thread thread = new Thread(new ClientHandler(client, data));
                thread.start();
            }
        } 
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}