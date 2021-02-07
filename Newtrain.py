from time import time
from pathlib import Path

from torch.utils.data import Dataset, DataLoader
from torch.utils.tensorboard import SummaryWriter

import time
import torch
import numpy as np
from pathlib import Path
from transformers import WEIGHTS_NAME, CONFIG_NAME
import pytorch_lightning as pl

import torch
from torch import nn
import pytorch_lightning as pl
from torch.utils.data import DataLoader, random_split
from torch.nn import functional as F
from torchvision.datasets import MNIST
from torchvision import datasets, transforms
import os

from data import IndicDataset, PadSequence
import model as M

from pytorch_lightning import Trainer

from config import replace, preEnc, preEncDec
#----------------------------------------------------------------------------






#-----------------------------------------------------------------------


def train_dataloader(self):
    train_loader = DataLoader(IndicDataset(tokenizers.src, tokenizers.tgt, config.data, True),
                              batch_size=config.batch_size,
                              shuffle=False,
                              collate_fn=pad_sequence)
    return train_loader


def prepare_data(self):
    from data import split_data
    split_data('C:/Users/Aboli/Downloads/itr-master/itr-master/itr/data/hin-eng/hin.txt', 'C:/Users/Aboli/Downloads/itr-master/itr-master/itr/data/hin-eng')
    rconf = preEncDec
    model, tokenizers, train_loader, eval_loader = gen_model_loaders(rconf)
    writer = SummaryWriter(rconf.log_dir)
    train_losses, val_losses, val_accs = run_train(rconf, model, train_loader, eval_loader, writer)

    model.save(tokenizers, rconf.model_output_dirs)



def gen_model_loaders(config):
    model, tokenizers = M.build_model(config)

    pad_sequence = PadSequence(tokenizers.src.pad_token_id, tokenizers.tgt.pad_token_id)



    return model, tokenizers


def val_dataloader():
    eval_loader = DataLoader(IndicDataset(tokenizers.src, tokenizers.tgt, config.data, False),
                             batch_size=config.eval_size,
                             shuffle=False,
                             collate_fn=pad_sequence)
    return eval_loader



############################################################



def init_seed():
    seed_val = 42
    np.random.seed(seed_val)
    torch.manual_seed(seed_val)
    torch.cuda.manual_seed_all(seed_val)


device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")


def save_model(model, output_dir):
    output_dir = Path(output_dir)
    # Step 1: Save a model, configuration and vocabulary that you have fine-tuned

    # If we have a distributed model, save only the encapsulated model
    # (it was wrapped in PyTorch DistributedDataParallel or DataParallel)
    model_to_save = model.module if hasattr(model, 'module') else model

    # If we save using the predefined names, we can load using `from_pretrained`
    output_model_file = output_dir / WEIGHTS_NAME
    output_config_file = output_dir / CONFIG_NAME

    torch.save(model_to_save.state_dict(), output_model_file)
    model_to_save.config.to_json_file(output_config_file)
    # src_tokenizer.save_vocabulary(output_dir)


def load_model():
    pass


# Function to calculate the accuracy of our predictions vs labels
def flat_accuracy(preds, labels):
    pred_flat = np.argmax(preds, axis=2).flatten()
    labels_flat = labels.flatten()
    # print (f'preds: {pred_flat}')
    # print (f'labels: {labels_flat}')

    return np.sum(np.equal(pred_flat, labels_flat)) / len(labels_flat)


def run_train(config, model, train_loader, eval_loader, writer):
    init_seed()
    #optimizer = torch.optim.Adam(model.parameters(), lr=config.lr)
    #scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(optimizer, len(train_loader), eta_min=config.lr)

    training_loss_values = []
    validation_loss_values = []
    validation_accuracy_values = []

    for epoch in range(config.epochs):

        trainer = Trainer()
        trainer.fit(model)

        print('======== Epoch {:} / {:} ========'.format(epoch + 1, config.epochs))
        start_time = time.time()

        total_loss = 0

        for batch_no, batch in enumerate(train_loader):
            source = batch[0].to(device)
            target = batch[1].to(device)

            model.zero_grad()

            loss, logits = model(source, target)
            total_loss += loss.item()

            logits = logits.detach().cpu().numpy()
            label_ids = target.to('cpu').numpy()

            loss.backward()

            optimizer.step()
            scheduler.step()

        # Logging the loss and accuracy (below) in Tensorboard
        avg_train_loss = total_loss / len(train_loader)
        training_loss_values.append(avg_train_loss)

        for name, weights in model.named_parameters():
            writer.add_histogram(name, weights, epoch)

        writer.add_scalar('Train/Loss', avg_train_loss, epoch)

        print("Average training loss: {0:.2f}".format(avg_train_loss))
        print("Running Validation...")

        model.eval()

        eval_loss, eval_accuracy = 0, 0
        nb_eval_steps = 0

        for batch_no, batch in enumerate(eval_loader):
            source = batch[0].to(device)
            target = batch[1].to(device)

            with torch.no_grad():
                loss, logits = model(source, target)

            logits = logits.detach().cpu().numpy()
            label_ids = target.to('cpu').numpy()

            tmp_eval_accuracy = flat_accuracy(logits, label_ids)
            eval_accuracy += tmp_eval_accuracy
            eval_loss += loss

            nb_eval_steps += 1

        avg_valid_acc = eval_accuracy / nb_eval_steps
        avg_valid_loss = eval_loss / nb_eval_steps
        validation_loss_values.append(avg_valid_loss)
        validation_accuracy_values.append(avg_valid_acc)

        writer.add_scalar('Valid/Loss', avg_valid_loss, epoch)
        writer.add_scalar('Valid/Accuracy', avg_valid_acc, epoch)
        writer.flush()

        print("Avg Val Accuracy: {0:.2f}".format(avg_valid_acc))
        print("Average Val Loss: {0:.2f}".format(avg_valid_loss))
        print("Time taken by epoch: {0:.2f}".format(time.time() - start_time))

    return training_loss_values, validation_loss_values, validation_accuracy_values


#tensorboard — logdir=./lightning_logs



if __name__ == '__main__':
    net = Net()
    trainer = Trainer()
    trainer.fit(net)