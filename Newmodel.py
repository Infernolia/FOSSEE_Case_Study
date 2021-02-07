import torch
from torch import nn
from transformers import BertConfig, BertModel, BertForMaskedLM, BertTokenizer
import pytorch_lightning as pl
from easydict import EasyDict as ED
from dataclasses import dataclass
from dataclasses import replace
from pathlib import Path
from easydict import EasyDict as ED


@dataclass
class Decoder:
    pretrained = True

@dataclass
class Encoder:
    pretrained = True

@dataclass
class Config:
    exp_name: str
    data: str
    lang: str

    base_log_dir: str = 'C:/Users/Aboli/Downloads/itr-master/itr-master/itr/logs'
    base_output_dir: str = 'C:/Users/Aboli/Downloads/itr-master/itr-master/itr/output'

    #embed_dim: int = 100
    hidden_size: int = 256
    intermediate_size: int = 512
    num_attention_heads: int = 1
    num_hidden_layers: int = 1
    hidden_act: str = 'gelu'
    dropout_prob: float = 0.1

    epochs: int = 1
    batch_size: int = 16
    eval_size: int = 16
    lr: float = 1e-3


    decoder: Decoder = Decoder()
    encoder: Encoder = Encoder()

    def __post_init__(self):
        self.log_dir = Path(self.base_log_dir) / self.exp_name
        self.log_dir.mkdir(parents=False, exist_ok=True)

        output_dir = Path(self.base_output_dir) / self.exp_name
        self.model_output_dirs = ED({})

        for m in ['encoder', 'decoder']:
            out = output_dir / m
            out.mkdir(parents=True, exist_ok=True)
            self.model_output_dirs[m] = out


#hc1 = Config(data=r'C:/Users/Aboli/Downloads/itr-master/itr-master/itr/data/hin-eng/', exp_name='default', lang='hi')

hc1 = Config(data='C:/Users/Aboli/Downloads/itr-master/itr-master/itr/data/hin-eng/',
                    exp_name='default',
                    lang='hi')



#increasing hidden size needs change in lr, others only change time
hc20 = replace(hc1,
                    num_hidden_layers=6,
                    num_attention_heads=8
                )
hc21 = replace(hc20, num_hidden_layers=12) #learns ok
hc22 = replace(hc21, num_attention_heads=12, hidden_size=288) #learns ok
hc23 = replace(hc21, num_attention_heads=12, hidden_size=384, lr=5e-4) #learns ok, 1e-4 too small

hc24 = replace(hc23, intermediate_size=3072, lr=1e-4)

preEnc = replace(hc24, epochs=5, exp_name='pretrained-enc')
preEncDec = replace(hc24, epochs=2, exp_name='pretrained-enc-dec')



#-------------------------------------------------------------------------------------











#------------------------------------------------------------------------------------------









class Net(pl.LightningModule):

    def __init__(self, encoder, decoder):
        super().__init__()

        # Creating encoder and decoder with their respective embeddings.
        self.encoder = encoder
        self.decoder = decoder

    def forward(self, encoder_input_ids, decoder_input_ids):
        encoder_hidden_states = self.encoder(encoder_input_ids)[0]
        loss, logits = self.decoder(decoder_input_ids,
                                    encoder_hidden_states=encoder_hidden_states,
                                    masked_lm_labels=decoder_input_ids)

        return loss, logits

    #def save(self, tokenizers, output_dirs):
    #    from train_util import save_model

    #   save_model(self.encoder, output_dirs.encoder)
    #    save_model(self.decoder, output_dirs.decoder)


    def configure_optimizers():
        src_tokenizer = BertTokenizer.from_pretrained('bert-base-multilingual-cased')
        tgt_tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')

        tgt_tokenizer.bos_token = '<s>'
        tgt_tokenizer.eos_token = '</s>'

        # hidden_size and intermediate_size are both wrt all the attention heads.
        # Should be divisible by num_attention_heads
        encoder_config = BertConfig(vocab_size=src_tokenizer.vocab_size,
                                    hidden_size=config.hidden_size,
                                    num_hidden_layers=config.num_hidden_layers,
                                    num_attention_heads=config.num_attention_heads,
                                    intermediate_size=config.intermediate_size,
                                    hidden_act=config.hidden_act,
                                    hidden_dropout_prob=config.dropout_prob,
                                    attention_probs_dropout_prob=config.dropout_prob,
                                    max_position_embeddings=512,
                                    type_vocab_size=2,
                                    initializer_range=0.02,
                                    layer_norm_eps=1e-12)

        decoder_config = BertConfig(vocab_size=tgt_tokenizer.vocab_size,
                                    hidden_size=config.hidden_size,
                                    num_hidden_layers=config.num_hidden_layers,
                                    num_attention_heads=config.num_attention_heads,
                                    intermediate_size=config.intermediate_size,
                                    hidden_act=config.hidden_act,
                                    hidden_dropout_prob=config.dropout_prob,
                                    attention_probs_dropout_prob=config.dropout_prob,
                                    max_position_embeddings=512,
                                    type_vocab_size=2,
                                    initializer_range=0.02,
                                    layer_norm_eps=1e-12,
                                    is_decoder=True)

        # Create encoder and decoder embedding layers.
        encoder_embeddings = torch.nn.Embedding(src_tokenizer.vocab_size, config.hidden_size,
                                                padding_idx=src_tokenizer.pad_token_id)
        decoder_embeddings = torch.nn.Embedding(tgt_tokenizer.vocab_size, config.hidden_size,
                                                padding_idx=tgt_tokenizer.pad_token_id)

        encoder = BertModel(encoder_config)
        encoder.set_input_embeddings(encoder_embeddings.cuda())

        decoder = BertForMaskedLM(decoder_config)
        decoder.set_input_embeddings(decoder_embeddings.cuda())

        model = TranslationModel(encoder, decoder)
        model.cuda()

        tokenizers = ED({'src': src_tokenizer, 'tgt': tgt_tokenizer})
        return model, tokenizers










