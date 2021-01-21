import speech_recognition as sr

mic_name = "sysdefault"
sample_rate = 48000#Sample rate is how often values are recorded 
chunk_size = 2048 #Chunk is like a buffer. It stores 2048 samples (bytes of data)

r = sr.Recognizer()
mic_list = sr.Microphone.list_microphone_names() 
for i, microphone_name in enumerate(mic_list): 
    if microphone_name == mic_name: 
        device_id = i 

def getAnswer(q) :    
    with sr.Microphone(device_index = device_id, sample_rate = sample_rate,  
                            chunk_size = chunk_size) as source: 

        r.adjust_for_ambient_noise(source) 

        print("Say the answer: ")
        audio = r.listen(source) #listens for the user's input 
            
        try: #error occurs when google could not understand what was said 
            text = r.recognize_google(audio) 
            return text

        except sr.UnknownValueError: 
            print("Google Speech Recognition could not understand audio") 
            return ""
        
        except sr.RequestError as e: 
            print("Could not request results from Google Speech Recognition service; {0}".format(e)) 
            return ""