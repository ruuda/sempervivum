%.webp: %.png
	# Compress to WebP, 1000 pixels wide, at quality level 95 for color and 95 for
	# alpha data. -m 6 indicates that we use the best (and slowest) possible
	# compression.
	cwebp -q 95 -alpha_q 95 -m 6 -resize 1000 0 $^ -o $@

all: $(patsubst %.png, %.webp, $(wildcard *.png))
