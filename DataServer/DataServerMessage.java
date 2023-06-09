public class DataServerMessage {
    //private Map<Integer, Integer> clock;
    private boolean inner;
    private String from;
    private String type;
    private String message;

    public DataServerMessage(){}

    public DataServerMessage(boolean inner, String from, String type, String message){
        //this.clock = parseClock(clockStr);
        this.inner = inner;
        this.from = from;
        this.type = type;
        this.message = message;
    }

    public DataServerMessage(String[] command){
        if (command.length == 4){
            if (command[0].equals("INNER")){
                this.inner = true;
            }
            else if (command[0].equals("OUTER"))
                this.inner = false;

            this.from = command[1];
            this.type = command[2];
            this.message = command[3];
        }
        else{
            this.message = null;
        }
    }

    //public Map<Integer, Integer> getClock(){ return this.clock; }
    public boolean getInner(){ return this.inner; }
    public String getFrom(){ return this.from; }
    public String getType(){ return this.type; }
    public String getMessage(){ return this.message; }

    public void setInner(boolean inner){ this.inner = inner; }
    public void setFrom(String from){ this.from = from; }
    public void setType(String type){ this.type = type; }
    public void setMessage(String msg){ this.message = msg; }


    // INNER!read!1
    public String constructMessage(){
        StringBuilder sb;
        if (inner)
            sb = new StringBuilder("INNER!");
        else
            sb = new StringBuilder("OUTER!");

        //sb.append("clock:{");
        //for (Map.Entry<Integer,Integer> entry: clock.entrySet()){
        //    sb.append("(" + entry.getKey() + "," + entry.getValue() + ")");
        //}
        //sb.append("}\n");
        sb.append(from + "!");
        sb.append(type + "!");
        sb.append(message);
        
        return sb.toString();
    }

    /* 
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
    */
}