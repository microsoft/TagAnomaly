import sys
from os import listdir
from os.path import isfile, join
import pandas as pd


def concat_labels(files_path, output_file_path):
    """
    Concatenates multiple CSVs with the same structure
    :param files_path: The directory in which CSVs are placed.
    :param output_file_path: The path to the concatenated output file
    :return:
    """
    onlyfiles = [f for f in listdir(files_path) if isfile(join(files_path, f))]

    appended_data = []
    for file in onlyfiles:
        try:
            tempdf = pd.read_csv(join(files_path, file), index_col=None)
            appended_data.append(tempdf)
        except Exception:
            print("Failed to read file " + file)

    labels = pd.concat(appended_data, axis=0)
    labels.to_csv(output_file_path)
    return labels


if __name__ == '__main__':
    if len(sys.argv) != 3:
        raise Exception("Please add the path to the labeled CSVs and a path to the output file")
    concat_labels(sys.argv[1], sys.argv[2])
