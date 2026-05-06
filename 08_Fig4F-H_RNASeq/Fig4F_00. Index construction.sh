#! /bin/bash
#SBATCH --job-name=v3_hisat2_index
#SBATCH --cpus-per-task=8
#SBATCH --mem=120G
#SBATCH --partition=medium
#SBATCH --mail-user=rz40@st-andrews.ac.uk
#SBATCH --mail-type=END,FAIL
#SBATCH --output %x_%j.out
#SBATCH --error %X_%j.err
 
echo "HOSTNAME: "hostname""
echo "Start Time: $(date)"
 
#########################################
## Hisat2 Index                   ##
#########################################
hisat2-build /home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/01.v3.genome/TOC.asm.scaffold.fasta \
/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/02.v3.index
