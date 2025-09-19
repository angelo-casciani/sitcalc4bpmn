from argparse import ArgumentParser
import os

from parser import Parser
from translator import Translator


def parse_arguments():
    parser = ArgumentParser(description="Parse and translate BPMN to IndiGolog.")
    parser.add_argument('--bpmn', type=str, required=True, help='Name of the BPMN model to translate.')
    args = parser.parse_args()

    return args


if __name__ == "__main__":
    args = parse_arguments()
    bpmn_name = args.bpmn
    bpmn_file = os.path.join(os.path.dirname(__file__), '..', 'models', f"{args.bpmn}.bpmn")
    pl_model_dir = os.path.join(os.path.dirname(__file__), '..', 'pl_models', f'{bpmn_name}')
    os.makedirs(pl_model_dir, exist_ok=True)
    pl_file = os.path.join(pl_model_dir, f'{bpmn_name}.pl')
    main_pl_file = os.path.join(pl_model_dir, 'main.pl')

    parser = Parser(bpmn_file)
    print(parser.pools)
    print(parser.tasks)
    print(parser.start_events)
    print(parser.end_events)
    print(parser.throw_message_events)
    print(parser.catch_message_events)
    print(parser.flows)
    print(parser.data_objects)
    print(parser.data_associations)
    print(parser.xor_gateways)
    print(parser.event_based_gateways)
    print(parser.parallel_gateways)

    translator = Translator(bpmn_name, main_pl_file, pl_file, parser)