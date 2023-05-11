import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DataServerMessage {
    private String from;
    private Map<Integer, Integer> clock;
    private String type;
    private String message;

    public DataServerMessage(String from, String clockStr, String type, String message){
        this.from = from.substring(5, from.length());
        this.clock = parseClock(clockStr);
        this.type = type.substring(5, type.length());
        this.message = message.substring(8, message.length());
    }

    public DataServerMessage(String from, Map<Integer, Integer> clock, String type, String message){
        this.from = from;
        this.clock = clock;
        this.type = type;
        this.message = message;
    }

    public String getFrom(){ return this.from; }
    public Map<Integer, Integer> getClock(){ return this.clock; }
    public String getType(){ return this.type; }
    public String getMessage(){ return this.message; }

    public String constructMessage(){
        StringBuilder sb = new StringBuilder("SERVER_MSG\n");

        sb.append("from:" + from + "\n");
        sb.append("clock:{");
        for (Map.Entry<Integer,Integer> entry: clock.entrySet()){
            sb.append("(" + entry.getKey() + "," + entry.getValue() + ")");
        }
        sb.append("}\n");
        sb.append("type:" + type + "\n");
        sb.append("message:" + message + "\n");
        
        return sb.toString();
    }

    public DataServerMessage parseMessage(String str){
        String lines[] = str.split("\n");
        String from = lines[1];
        String clockStr = lines[2];
        String type = lines[3];
        String message = lines[4];

        return parseMessage(from, clockStr, type, message);
    }

    public DataServerMessage parseMessage(String from, String clockStr, String type, String message){
        from = from.substring(5, from.length());
        Map<Integer, Integer> clock = parseClock(clockStr);
        type = type.substring(5, type.length());
        message = message.substring(8, message.length());

        return new DataServerMessage(from, clock, type, message);
    }

    private Map<Integer, Integer> parseClock(String str){
        List<int[]> indexes = new ArrayList<>();
        String clockStr = str.substring(7, str.length()-1);
        int startE = 0;
        int endE = 0; 
        for (int i = 0; i < clockStr.length(); i++){
            if (clockStr.charAt(i) == '('){
                startE = i;
            }
            else if (clockStr.charAt(i) == ')'){
                endE = i;
                int[] v = {startE + 1, endE};
                indexes.add(v);
            }
        }

        Map<Integer, Integer> clock = new HashMap<>();
        for (int[] pair: indexes){
            String instance = clockStr.substring(pair[0], pair[1]);
            String nrs[] = instance.split(",");
            clock.put(Integer.parseInt(nrs[0]), Integer.parseInt(nrs[1]));
        }

        return clock;
    }
}