#! /bin/bash
#SBATCH --job-name=2.1.7.StringTie_Merge_V3_nopre
#SBATCH --cpus-per-task=8
#SBATCH --mem=16G
#SBATCH --partition=medium
#SBATCH --mail-user=rz40@st-andrews.ac.uk
#SBATCH --mail-type=END,FAIL
#SBATCH --output %x_%j.out
#SBATCH --error %x_%j.err
 
echo "HOSTNAME: $HOSTNAME"
echo "Start Date: $(date)"

#########################################
## StringTie_Merge      ##
#########################################
genome_gtfdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/01.v3.genome'
sample_gtfdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/2.1.7_03_StringTie_Alignment_NoPre'
outdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/2.1.7_04_StringTie_Merged_NoPre'

ls $sample_gtfdir/*gtf > mergelist.txt
stringtie --merge -p 8 -e -G $genome_gtfdir/TOC.asm.scaffold.gene.gff3 -o $outdir/stringtie_merged.gtf mergelist.txt 


echo "End Date: $(date)"
