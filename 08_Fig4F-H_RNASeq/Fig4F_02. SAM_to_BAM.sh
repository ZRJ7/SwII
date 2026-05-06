#! /bin/bash
#SBATCH --job-name=Sw_RNAseq_SAM_to_BAM_V3
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
## sam to bam                   ##
#########################################

samdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/01_HISAT2_ALIGNMENT'
bamdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/02_SAM_to_BAM'

cat $samdir/samfilelist | while read id
do

    samtools sort -@ 8 -o $bamdir/${id%%.*}.bam $samdir/$id
done

