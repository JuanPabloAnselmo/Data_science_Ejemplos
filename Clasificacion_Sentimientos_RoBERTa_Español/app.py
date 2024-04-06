import gradio as gr
from transformers import AutoTokenizer, AutoModel
import torch
from torch import nn
import torch.nn.functional as F


# Creo la clase del modelo
class RobertaModel(nn.Module):

    def __init__(self, n_classes: int = 2):
        super().__init__()
        self.roberta = AutoModel.from_pretrained("xlm-roberta-base") #Modelo transformer
        self.dropout = nn.Dropout(p=0.3) #Dropout para disminuir el overfitting
        self.linear = nn.Linear(self.roberta.config.hidden_size, self.roberta.config.hidden_size) # 1er capa linear
        self.classification = nn.Linear(self.roberta.config.hidden_size, n_classes) # 2da capa linear
    
    def forward(self, input_ids, attention_mask):
        #Roberta layer
        cls_output = self.roberta(input_ids=input_ids, attention_mask=attention_mask) 
        pooled_output = torch.mean(cls_output.last_hidden_state, 1)

        
        # NN     
        pooled_output = self.linear(pooled_output) # Primera capa
        pooled_output = F.relu(pooled_output) # Funcion de activacion relu
        pooled_output = self.dropout(pooled_output) # Dropout        
        output = self.classification(pooled_output) #Segunda capa

        return output

model = RobertaModel()

# Cargo los pesos ya entrenados del modelo

model.load_state_dict(
    torch.load(
        f="Modelo_Amazon_review.pt",
        map_location=torch.device("cpu") # Cambio modelo a cpu
    )
)

# Cargo modelo para el Tokenizer
        
tokenizer = AutoTokenizer.from_pretrained('xlm-roberta-base')


# Creo  funcion para predecir

def predict(review_text):
    pred = {}
    
    encoding_review = tokenizer.encode_plus(
        review_text,
        max_length = 250, #Maximo del larogo del texto
        truncation = True, # Truncar texto
        add_special_tokens = True, #Agregar tokens especiales
        return_token_type_ids = False, # Que no devuelva los ids de esos tokens
        padding = "max_length", # Que realice padding hasta el maximo alrgo
        return_attention_mask = True, #Que devuelva la mascara de atencion
        return_tensors = 'pt' # Que los tensores que devuelve sean Pytorch
        )
    
    input_ids = encoding_review['input_ids']
    attention_mask = encoding_review['attention_mask']
    output = model(input_ids, attention_mask)    
    _, prediction = torch.max(output, dim=1)
    if prediction == 0:
        pred["label"] = "Negativo"
        pred["score"] = f"{torch.softmax(output, dim=1)[0][0].item()*100:.2f}%"
    else:
        pred["label"] = "Positivo"
        pred["score"] = f"{torch.softmax(output, dim=1)[0][1].item()*100:.2f}%"
    
    return pred["label"], pred["score"]

#  Funcion para crear interfaz

amazon_app = gr.Interface(
    fn=predict,
    inputs=gr.Textbox(label="Introduce tu reseña aquí:", placeholder="Escribe aquí..."),
    outputs=[gr.Label(label="Predicción"), gr.Label(label="Puntaje")],
    title="Análisis de sentimientos de reseñas de productos en Español",
    description="Ingresa una reseña de algun producto y obtén una predicción sobre si su sentimiento es positivo o negativo. (Max. 250 caracteres)",
    theme="Agora",
    #layout="vertical",
    #interpretation="default",
    allow_flagging=False,
    analytics_enabled=False
)

# Ejecuta la aplicación
amazon_app.launch()