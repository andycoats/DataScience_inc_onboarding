main = function(time){
    # This is an example function to demonstrate the Scheduled Jobs function of the DataScience Cloud
    paste("New pickup data ingested at", time)
    }

input_time = format(Sys.time(), "%H:%M %m/%d/%Y")
main(input_time)